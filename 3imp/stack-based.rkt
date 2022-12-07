#lang racket
(require rackunit)

;; VM instructions
(struct Halt () #:transparent)
(struct Cst (val x) #:transparent)
(struct Ref (n m x) #:transparent)
(struct Closure (code x) #:transparent)
(struct Test (thn els) #:transparent)
(struct Assign (n m x) #:transparent)
(struct Frame (ret x) #:transparent)
(struct Arg (x) #:transparent)
(struct Apply () #:transparent)
(struct Ret (n) #:transparent)
(struct Prim (proc n x) #:transparent)

;;
(struct functional (code link) #:transparent)


(define (compile expr env nxt)
  (match expr
	[(or '+ '- '* '/) ;;primitives
	 (Prim (match expr ['+ +] ['- -] ['* *] ['/ /]) 2 nxt)]
	[(? symbol?) (compile-lookup expr env (lambda (n m) (Ref n m nxt)))]
	[`(lambda ,vs ,body)
	  (Closure (compile body 
						(cons vs env)
						(Ret (+ 1 (length vs))))
			   nxt)]
	[`(if ,pred ,thn ,els)
	  (let ([thnc (compile thn env nxt)]
			[elsc (compile els env nxt)])
		(compile pred env (Test thnc elsc)))]
	[`(set! ,var ,val)
	  (compile-lookup var env (lambda (n m)
								(compile val env (Assign n m nxt))))]
	[`(,f ,args ...)
	  (for/fold ([c (compile f env (Apply))] #:result (Frame nxt c))
		([arg args])
		(compile arg env (Arg c)))]
	[_ (Cst expr nxt)]))


(define (compile-lookup var env ret)
  (let outter ([o env] [n 0])
	(let inner ([i (car o)] [m 0])
	  (cond
		[(null? i) (outter (cdr o) (+ n 1))]
		[(eq? (car i) var) (ret n m)]
		[else (inner (cdr i) (+ m 1))]))))

;; a -- accumulator
;; x -- instr
;; e -- frame pointer
;; s -- satck pointer
(define (VM a x e s)
  (define stack (make-vector 1000))
  (define (push e s) (begin (vector-set! stack s e) (+ s 1)))
  (define (index s i) (vector-ref stack (- (- s i) 1)))
  (define (index-set! s i v) (vector-set! stack (- (- s i) 1) v))
  (define (find-link n e)
	(if (= n 0) e (find-link (- n 1) (index e -1))))


  (match x
	[(? Halt?) a]
	[(Cst val nxt) (VM val nxt e s)]
	[(Prim proc n x) (VM (Prim proc n '()) x e s)]
	[(Ref n m nxt) (VM (index (find-link n e) m) x e s)]
	[(Closure code nxt) (VM (functional code e) x e s)]
	[(Test thn els) (VM a (if a thn els) e s)]
	[(Assign n m nxt) 
	 (index-set! (find-link n e) m a)
	 (VM a nxt e s)]
	[(Frame ret nxt) (VM a nxt e (push ret (push e s)))]
	[(Arg nxt) (VM a nxt e (push a s))]
	[(? Apply?) 
	 (match a
	   [(functional code link)
		(VM a code s (push link))]
	   [(Prim proc n _)
		(let ([args (for/fold ([args '()])
					  ([i (in-range (- n 1) -1 -1)])
					  (cons (index s i) args))])
		  (VM (apply proc args) (Ret n) e s))])]
	[(Ret n)
	 (let ([s (- s n)])
	   (VM a (index s 0) (index s 1) (- s 2)))]))


(module+ test
  (define (ckeq e v)
	(check-equal? (VM 0 (compile e '() (Halt)) 0 0) v))

  (test-case
	"basic"
	(ckeq '3 3)
	(ckeq '(+ 3 4) 7)
	(ckeq '(if #t 42 -1) 42)
	(ckeq '(if #f 0 1) 1)
	(ckeq '(if (if #f #t #f) 0 1) 1)
	(ckeq '(+ (if #t 2 -2) (if #f 4 2)) 4)
	(ckeq '(begin (+ 2 3) 10) 10)
	)

  (test-case 
	"lambda"
	(ckeq '((lambda (x) x) 4) 4)
	(ckeq '(((lambda (x) (lambda (y) (+ x y))) 5) 6) 11)
	(ckeq '((lambda (x y) (begin (set! x 0) (+ x y))) 5 10) 10)
	(ckeq '(((lambda (a b) (lambda (b) (- a b))) 10 2) 5) 5)
	)
)

