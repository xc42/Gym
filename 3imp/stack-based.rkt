#lang racket
(require rackunit)

;; VM instructions
(struct Halt () #:transparent)
(struct Cst (val x) #:transparent)
;(struct Ref (n m x) #:transparent)
(struct Ref-Local (n x) #:transparent)
(struct Ref-Free (n x) #:transparent)
(struct Closure (n-free code x) #:transparent)
(struct Test (thn els) #:transparent)
(struct Assign-Local (n x) #:transparent)
(struct Assign-Free (n x) #:transparent)
(struct Frame (ret x) #:transparent)
(struct Arg (x) #:transparent)
(struct Apply () #:transparent)
(struct Ret (n) #:transparent)
(struct Cont (x) #:transparent)
(struct Resume (s x) #:transparent)
(struct Prim (proc n x) #:transparent)

(struct ClosureV (fvs code) #:transparent)

(define primes (set '+ '- '* '/ '> '< '= '>= '<=))
(define (prime? op) (set-member? primes op)) 
(define (prime-arity op) 2)
(define (prime-op op)
  (let ([ns (make-base-namespace)])
	(eval op ns)))

(define (compile expr env nxt)
  (match expr
	[(? symbol?) 
	 (if (set-member? primes expr)
	   (Prim (prime-op expr) 2 nxt)
	   (compile-lookup expr env 
					   (lambda (i) (Ref-Local i nxt))
					   (lambda (i) (Ref-Free i nxt))))]
	[`(lambda ,vs ,body)
	  (let* ([fvs (free-vars expr)]
			 [n (length vs)]
			 [body-c (compile body 
							  (cons vs (set->list fvs)) 
							  (Ret n))])
		(for/fold ([acc (Closure (set-count fvs) body-c nxt)])
		  ([fv fvs]
		   [i (in-naturals)])
		  (compile-lookup fv env 
						  (lambda (i) (Ref-Local i (Arg acc)))
						  (lambda (i) (Ref-Free  i (Arg acc))))))]
	[`(let ([,vars ,vals] ...) ,body)
	  (compile `((lambda ,vars ,body) ,@vals) env nxt)] ;;TODO
	[`(if ,pred ,thn ,els)
	  (let ([thnc (compile thn env nxt)]
			[elsc (compile els env nxt)])
		(compile pred env (Test thnc elsc)))]
	[`(begin ,es ...) (foldr (lambda (expr x) (compile expr env x)) nxt es)]
	[`(set! ,var ,val)
	  (compile val env 
			   (compile-lookup var env 
							   (lambda (i) (Assign-Local i nxt))
							   (lambda (i) (Assign-Free i nxt))))]
	[`(call/cc ,f) 
	  (Frame nxt 
			 (Cont (Arg (compile f env (Apply)))))]
	[`(,f ,args ...)
	  (for/fold ([c (compile f env (Apply))] #:result (Frame nxt c))
		([arg args])
		(compile arg env (Arg c)))]
	[_ (Cst expr nxt)]))


(define (compile-lookup var env loc free)
  (cond
	[(index-of (car env) var) => loc] 
	[(index-of (cdr env) var) => free] 
	[else (error (format "~a not found in env: ~a" var env))]))

(define (free-vars expr)
  (define (collect e acc)
	(set-union (free-vars e) acc))

  (match expr
	[(? symbol?) #:when (not (prime? expr)) (set expr)]
	[`(lambda ,vars ,body) (set-subtract (free-vars body) (apply set vars))]
	[`(let ([,vars ,vals] ...) ,body)
	  (set-union (set-subtract (free-vars body) (apply set vars))
				 (foldl collect (set) vals))]
	[`(if ,e1 ,e2 ,e3) 
	  (set-union (free-vars e1) (free-vars e2) (free-vars e3))]
	[`(begin ,es ...) (foldl collect (set) es)]
	[`(set! ,var ,val) (set-add (free-vars val) var)]
	[`(call/cc ,f) (free-vars f)]
	[`(,es ...) (foldl collect (set) es)]
	[_ (set)]))


(define VM 
  (class object%
	(super-new)

	(define stack (make-vector 500))

	(define (push e s) 
	  (vector-set! stack s e) 
	  (+ s 1))

	(define (index s i) (vector-ref stack (- (- s i) 1)))

	(define (index-free c i) (vector-ref (ClosureV-fvs c) i))

	(define (index-set! s i v) (vector-set! stack (- (- s i) 1) v))

	(define (index-set-free! c i v) (vector-set! (ClosureV-fvs c) i v))

	(define (find-link n e)
	  (if (= n 0) 
		e 
		(find-link (- n 1) (index e -1))))

	(define (continuation s)
	  (ClosureV '() (Ref-Local 0 
							   (Resume (save-stack s) (Ret 0)))))

	(define (save-stack s) (vector-copy stack 0 s))

	(define (restore-stack stk) 
	  (vector-copy! stack 0 stk)
	  (vector-length stk))


	;; a -- accumulator
	;; x -- instr
	;; f -- frame pointer
	;; c -- current closure
	;; s -- satck pointer
	(define/public (run a x f c s)
	  #;(displayln (format "-----------\n Reg a: ~a\n Reg x: ~a\n Reg f: ~a\n Reg c: ~a\n Reg s: ~a\n Stack: ~a\n----------\n" 
						 a x f c s (vector-take stack 20)))
	  (match x
		[(? Halt?) a]
		[(Cst val x) (run val x f c s)]
		[(Prim proc n x) (run (Prim proc n '()) x f c s)]
		[(Ref-Local n x) (run (index f n) x f c s)]
		[(Ref-Free n x) (run (index-free c n) x f c s)]
		[(Closure n code x) 
		 (let ([cc (ClosureV (vector-copy stack (- s n) s) code)])
		   (run cc x f c (- s n)))]
		[(Test thn els) (run a (if a thn els) f c s)]
		[(Assign-Local n x) 
		 (begin 
		   (index-set! f n a)
		   (run a x f c s))]
		[(Assign-Free n x) 
		 (begin 
		   (index-set-free! c n a)
		   (run a x f c s))]
		[(Frame ret x) (run a x f c (push c (push f (push ret s))))]
		[(Arg x) (run a x f c (push a s))]
		[(Cont x) (run (continuation s) x f c s)]
		[(Resume stk x) (run a x f c (restore-stack stk))]
		[(? Apply?) 
		 (match a
		   [(ClosureV fvs code)
			(run a code s a s)]
		   [(Prim proc n _)
			(let ([args (for/fold ([args '()])
						  ([i (in-range (- n 1) -1 -1)])
						  (cons (index s i) args))])
			  (run (apply proc args) (Ret n) f c s))])]
		[(Ret n)
		 (let ([s (- s n)])
		   (run a (index s 2) (index s 1) (index s 0) (- s 3)))]))
	) ;;class
  );; VM


(module+ test
  (define-syntax-rule (ckeq e v)
	(check-equal? (send (new VM) run 0 (compile e '() (Halt)) 0 0 0) v))

  (test-case
	"basic"
	(ckeq '3 3)
	(ckeq '(+ 3 4) 7)
	(ckeq '(- 10 6) 4)
	(ckeq '(if #t 42 -1) 42)
	(ckeq '(if #f 0 1) 1)
	(ckeq '(if (if #f #t #f) 0 1) 1)
	(ckeq '(+ (if #t 2 -2) (if #f 4 2)) 4)
	(ckeq '(begin (+ 2 3) 10) 10)
	)

  (test-case 
	"lambda"
	(ckeq '((lambda (x) x) 4) 4)
	(ckeq '((lambda (x y z) (* x (- y z))) 2 15 5) 20)
	(ckeq '(((lambda (x) (lambda (y) (+ x y))) 5) 6) 11)
	(ckeq '((((lambda (x) 
			   (lambda (y) 
				 (lambda (z)
				   (+ z (+ x y))))) 5) 6) 7) 18)
	(ckeq '((lambda (x y) (begin (set! x 0) (+ x y))) 5 10) 10)
	(ckeq '((lambda (f x) (f x)) (lambda (x) x) 42) 42)
	(ckeq '(((lambda (a b) (lambda (b) (- a b))) 10 2) 4) 6)
	(ckeq '(let ([compose (lambda (f g)
						 (lambda (x)
						   (f (g x))))]
				 [double (lambda (x) (+ x x))]
				 [square (lambda (x) (* x x))])
			 ((compose double square) 4))
		  32)
				 
	)

  (test-case
	"call/cc"
	(ckeq '(call/cc (lambda (k) (k 3))) 3)
	(ckeq '(+ 2 (call/cc (lambda (k) (k 3)))) 5)
	(ckeq '(+ 2 (- 2 (call/cc (lambda (k) (k 3))))) 1)
	(ckeq '(call/cc (lambda (k) (* 4 (k 42)))) 42)
	(ckeq '(let ([x 5])
			 (let ([y (call/cc (lambda (c)
							   (c x)))])
			 (- x y)))
		  0)
	(ckeq '(let ([f (lambda (resume x)
                              (+ x (call/cc (lambda (cf)
                                               (resume cf x)))))]
                         [g (lambda (resume x)
                              (resume (* x x)))])
                     (f g 5))
		  30)
	#;(ckeq '(let ([c0 -1]
				 [i 0]
				 [total 0])
			 (let ([y (call/cc (lambda (c)
								 (begin
								   (set! c0 c)
								   100)))])
			   (if (< i 10)
				 (begin
				   (set! total (+ total y))
				   (set! i (+ i 1))
				   (c0 i))
				 total)))
				   
		  145)
	)
)

