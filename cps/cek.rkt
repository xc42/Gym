#lang racket
(require rackunit)

(struct Env (kvs))
(struct Closure (ps body env))

(struct IdK ())
(struct LetK (vs es body env cont))
(struct AppK (f args arg-vals env cont))

(define (extend-env env k v)
  (match env
	[(Env kvs) (Env (cons (cons k v) kvs))]))
  
(define (apply-env env k)
  (match env
	[(Env kvs) (dict-ref kvs k)]))

(define (apply-cont cont val)
  (match cont
	[(? IdK?) val]
	[(LetK vs es body env k)
	 (let ([env^ (extend-env env (car vs) val)])
	   (if (null? es)
		 (interp-cps body env^ k)
		 (interp-cps (car es) env^ (LetK (cdr vs) (cdr es) body env k))))]
	[(AppK f args arg-vals env k)
	 (cond
	   [(null? args) (apply-proc f (reverse arg-vals))]
	   [else 
		(let ([cont^ (if (null? arg-vals)
						(AppK val (cdr args) '() env k)
						(AppK val (cdr args) (cons val arg-vals) env k))])
		   (interp-cps (car args) env cont^))])]
	))

(define (apply-proc proc arg-vals cont)
  (match proc
	[(Closure ps body env) 
	 (let ([env^ (for/fold ([acc-env env])
						([p ps]
						 [v arg-vals])
						(extend-env acc-env p v))])
	 (interp-cps body env^ cont))]
	[(? procedure?) ;primitive
	 (apply-cont cont (apply proc arg-vals))]))

(define (interp-cps expr env cont)
  (match expr
	[(? symbol?) (apply-cont cont (apply-env env expr))]
	[(? number?) (apply-cont cont expr)]
	[`(lambda ,ps ,body) (apply-cont cont (Closure ps body env))]
	[`(let ([,vs ,es] ...) ,body)
	  (interp-cps (car es) env (LetK vs (cdr es) body env cont))]
	[`(,f ,args ...) 
	  (interp-cps f env (AppK '() args '() env cont))]
	))


(module+ test
  (test-begin
	(let* ([env0 `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/))]
		   [interp (lambda (e) (interp-cps e env0 IdK))])
	  (check-equal? (interp '2) 2)
	  )))
