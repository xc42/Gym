#lang racket
(require rackunit)

;; VM Instructions
(struct Halt () #:transparent)
(struct Cst (val x) #:transparent)
(struct Ref (var x) #:transparent)
(struct Closure (ps body x) #:transparent)
(struct Test (thn els) #:transparent)
(struct Assign (var x) #:transparent)
(struct Cont (x))
(struct Resume (s var))
(struct Frame (ret x) #:transparent)
(struct Arg (x) #:transparent)
(struct Apply () #:transparent)
(struct Ret () #:transparent)


;; Values
(struct ClosureV (ps body env) #:transparent)
(struct CallFrame (ret env rib stk) #:transparent)


(define (compile e nxt)
  (match e
	[(? symbol?) (Ref e nxt)]
	[`(lambda ,ps ,body) 
	  (Closure ps (compile body (Ret)) nxt)]
	[`(if ,pred ,thn ,els) 
	  (compile pred (Test (compile thn nxt) (compile els nxt)))]
	[`(set! ,var ,val) 
	  (compile val (Assign var nxt))]
	[`(call/cc ,v) 
	  (let ([c (Cont (Arg (compile v (Apply))))])
		(if (Ret? nxt)
		  c
		  (Frame nxt c)))]
	[`(begin ,es ...) (foldr compile nxt es)]
	[`(let ([,vars ,vals] ...) ,body)

	  42] ;;TODO
	[`(,f ,args ...)
	  (for/fold ([c (compile f (Apply))] 
				 #:result (if (Ret? nxt) c (Frame nxt c)))
		([arg args])
		(compile arg (Arg c)))]
	[_ (Cst e nxt)]))


;; a -- accumulator
;; x -- current instruction
;; e -- enviroment
;; r -- argument rib
;; s -- control stack
(define (VM a x e r s)

  (define (lookup e v)
	(cond
	  [(null? e) (error (format "var ~a not found" v))]
	  [(dict-has-key? (car e) v) (dict-ref (car e) v)]
	  [else (lookup (cdr e) v)]))

  (define (set-val! e var val)
	(cond
	  [(null?  e) (error (format "var ~a not found" var))]
	  [(dict-has-key? (car e) var) (dict-set! (car e) var val)]
	  [else (set-val! (cdr e) var val)]))

  (define (extend e vars vals)
	(cons (make-hash (map cons vars vals)) e))


  (match x
	[(? Halt?) a]
	[(Cst v x) (VM v x e r s)]
	[(Ref v x) (VM (lookup e v) x e r s)]
	[(Closure ps body x) (VM (ClosureV ps body e) x e r s)]
	[(Test thn els) 
	 (if a 
	   (VM a thn e r s) 
	   (VM a els e r s))]
	[(Assign var x)
	 (set-val! e var a)
	 (VM a x e r s)]
	[(Frame ret x) (VM a x e '() (CallFrame ret e r s))]
	[(Arg x) (VM a x e (cons a r) s)]
	[(? Apply?) 
	 (match a
	   [(ClosureV ps body e) (VM a body (extend e ps r) '() s)]
	   [(? procedure?) ;;builtin procedures
		(VM (apply a r) (Ret) e '() s)])]
	[(Cont x) (VM (ClosureV '(v) (Resume s 'v) '()) x e r s)]
	[(Resume s var) (VM (lookup e var) (Ret) e r s)]
	[(? Ret?)
	 (match-let ([(CallFrame ret e r s) s])
	   (VM a ret e r s))]))

(module+ test
  (define env0 (list (make-hash `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/)))))
  (define (ckeq e val)
	(let ([codec (compile e (Halt))])
	  (check-equal? (VM '() codec env0 '() '()) val)))

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

  (test-case
	"call/cc"
	(ckeq '(+ 2 (call/cc (lambda (k) (k 3)))) 5)
	(ckeq '(+ 2 (- (call/cc (lambda (k) (k 3))))) -1)
	(ckeq '(call/cc (lambda (k) (* 4 (k 42)))) 42)
	)
  )
