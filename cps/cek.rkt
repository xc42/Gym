#lang racket
(require rackunit)
(require rackunit/text-ui)

;;AST
(struct Env (kvs))
(struct Closure (ps body env))

;;Continuation
(struct IdK ())
(struct IfK (thn els env cont))
(struct LetK (vs es body env cont))
(struct AppK (args vals env cont))
(struct TryK (catch-var catch-e env cont))
(struct ThrowK (saved-cont))

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
		 (interp-cps (car es) env^ (LetK (cdr vs) (cdr es) body env^ k))))]
	[(AppK args vals env k)
	 (cond
	   [(null? args) 
		(let ([vals-r (reverse (cons val vals))])
		  (apply-proc (car vals-r) (cdr vals-r) k))]
	   [else 
		 (interp-cps (car args) env (AppK (cdr args) (cons val vals) env k))])]
	[(IfK thn els env cont)
	 (if val
	   (interp-cps thn env cont)
	   (interp-cps els env cont))]
	[(TryK var catch-e env cont) (apply-cont cont val)]
	[(ThrowK k) 
	 (let find-handler ([saved-cont k])
	   (match saved-cont
		 [(TryK v catch-e env cont) (interp-cps catch-e (extend-env env v val) cont)]
		 [(? IdK?) (error "unhandled exception")]
		 [(LetK _ _ _ _ k) (find-handler k)]
		 [(AppK _ _ _ k) (find-handler k)]))]

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
	[`(if ,pred ,thn ,els) (interp-cps pred env (IfK thn els env cont))]
	[`(try ,try-e catch (,var) ,catch-e)
	  (interp-cps try-e env (TryK var catch-e env cont))]
	[`(throw ,exn)
	  (interp-cps exn env (ThrowK cont))]
	[`(,f ,args ...) 
	  (interp-cps f env (AppK args '() env cont))]
	))


(module+ test
  (define env0 (Env `((+ . ,+) (- . ,-) (* . ,*) (/ . ,/) 
					  (> . ,>) (< . ,<) (>= . ,>=) (<= . ,<=) (= . ,=))))
  (define k0 (IdK))
  (define (ckeq e val) (check-equal? (interp-cps e env0 k0) val))

  (run-tests
	(test-suite 
	  "CEK"
	  (test-case 
		"basic lang"
		(ckeq '2 2)
		(ckeq '(+ 2 3) 5)
		(ckeq '(let ([x 3]) (+ x 4)) 7)
		(ckeq '(let ([x 1] [y 3]) (- y x)) 2)
		(ckeq '(let ([db (lambda (x) (* x x))]) (db 4)) 16)
		(ckeq '(let ([x 3]) (let ([add3 (lambda (y) (+ y x))]) (add3 5))) 8))

	  (test-case 
		"try-catch"
		(ckeq '(try (+ 4 3) catch (ex) ex) 7)
		(ckeq '(let ([x 3]) (try (throw (* x x)) catch (ex) ex)) 9)
		(ckeq '(try (throw (lambda (x) (+ x 1))) catch (f) (f 9)) 10)
		(ckeq '(let ([f (lambda (x)
						  (throw x))])
				 (try (f 41)
					  catch (x) (+ x 1))) 42)
		(ckeq '(let ([g (lambda (x) (if (< x 0) (throw x) x))])
				 (let ([f (lambda (x) (+ 4 (g x)))])
				   (try (f -3) catch (ex) (- ex)))) 3))
	  ))
)
