#lang racket

(require multimethod)

(module+ test
  (require rackunit))

; ===== EVALUATOR =====

(define (eval-expr expr env)
  (match expr
    ; Numbers and strings returned as-is
    [(? number?) expr]
    [(? string?) expr]
    [(? null?) expr]
    ['true expr]
    ['false expr]

    ; Variables
    [(? symbol?) (lookup-variable expr env)]
    [`(set! ,var ,val) (set-variable! var val env)]

    [`(quote ,body) body]

    ; Define
    [`(define (,binding ,args ...) ,body ...) (list 'define-lambda binding 'args args 'body body)]
    [`(define ,binding ,val) (list 'define binding 'as val)]

    [`(lambda (,args ...) ,body ...) (compound-procedure args body)]

    [`(if ,condition ,consequent) (eval-if condition consequent null env)]
    [`(if ,condition ,consequent ,alternative) (eval-if condition consequent alternative env)]

    [`(begin ,body ...) (last (list-of-values (cdr body) env))]

    ; Apply form
    [`(,operator ,operands ...) (apply-proc (eval-expr operator env)
                                            env
                                            (list-of-values operands env))]

    [_ (error "Unknown expression" expr)]
))

(define (eval-if condition consequent alternative env)
  (if (true? (eval-expr condition env))
      (eval-expr consequent env)
      (eval-expr alternative env)))

(define (true? expr)
  (not (eq? 'false expr)))

(define (false? expr)
  (eq? 'false expr))

(define (list-of-values exprs env)
  ; Evaluate all of the items in the list. If the list is null, return an empty list
  (if (null? exprs)
      '()
      (cons (eval-expr (car exprs) env)
            (list-of-values (cdr exprs) env))))

; ===== PROCEDURES =====

; Primitive procedure that maps directly to a racket function
(struct primitive-procedure (func))

; A user-defined procedure
(struct compound-procedure (args body))

(define-generic (apply-proc proc _ _))

; Apply for primitive procedures
(define-instance ((apply-proc primitive-procedure) proc env args)
  (apply (primitive-procedure-func proc) args))

; Apply for regular procedures
(define-instance ((apply-proc compound-procedure) proc env args)
  (let ((newenv (extend-environment (compound-procedure-args proc) (list-of-values args env) env)))
    (last (list-of-values (compound-procedure-body proc) newenv))))

(define (procedure? p)
  (or (primitive-procedure? p)
      (compound-procedure? p)))

; ===== ENVIRONMENT ======

(define (lookup-variable-in-frame var frame)
  (hash-ref frame var null))

(define (make-frame vars vals)
  ; Make a new hash map from the given keys and values
  (make-hash (map (λ (i j) (cons i j)) vars vals)))

(define (extend-environment vars vals base-env)
  ; Extend the given environment with the given keys and values
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable var env)
  ; Find the variable in the environment, or throw an error if it doesn't exist
  (define (env-loop env)
    (if (or (null? env) (eq? env '()))
        (error "Unbound variable" var)
        (let* ((frame (car env))
               (val (lookup-variable-in-frame var frame)))
          (if (null? val)
              (env-loop (cdr env))
              val))))
  (env-loop env))

(define (set-variable! var val env)
  ; Set the variable value in the environment, or throw an error if it doesn't exist
  (define (env-loop env)
    (if (eq? env '())
        (error "Unbound variable" var)
        (let* ((frame (car env)))
          (if (hash-has-key? frame var)
              (begin
                (hash-set! frame var val)
                env)
              (env-loop (cdr env))))))
  (env-loop env))

(define (make-default-environment)
  ; Create a default global environment with primitive procedures
  (let* ((prim-procs
         `((cons ,cons)
           (car ,car)
           (cdr ,cdr)
           (display ,display)
           (+ ,+)
           (- ,-)
           (* ,*)
           (/ ,/)))
        (proc-keys (map (λ (x) (car x)) prim-procs))
        (proc-values (map (λ (x) (primitive-procedure (cadr x))) prim-procs))
        (env (extend-environment proc-keys proc-values '())))
    env))

; ===== TESTS =====

(module+ test

  ; Test environment get
  (check-equal? (lookup-variable
                 'a
                 (extend-environment '(a) '(1) '()))
                 1)
  
  (check-equal? (lookup-variable
                 'b
                 (extend-environment '(b) '(2) (extend-environment '(a) '(1) '())))
                 2)

  ; Test environment set
  (check-exn
   exn:fail?
   (λ () (lookup-variable 'c '())))

  (check-equal? (lookup-variable 'a (set-variable!
                                     'a
                                     123
                                     (extend-environment '(a) '(1) '())))
                123)

  (check-exn
   exn:fail?
   (λ () (set-variable! 'b 123 '())))

  ; Test evaluator

  ; Number -> Number
  (check-equal? (eval-expr 123 '()) 123)

  ; String -> String
  (check-equal? (eval-expr "Test" '()) "Test")

  ; Null -> Null
  (check-equal? (eval-expr null '()) null)

  ; Symbol lookup
  (check-equal? (eval-expr 'a (extend-environment '(a) '(123) '())) 123)

  ; Begin form
  (check-equal? (eval-expr '(begin 1 2) '()) 2)
  
  ; Quote
  (check-equal? (eval-expr '(quote (1 2 3)) '()) '(1 2 3))

  ; If, 1 arm
  (check-equal? (eval-expr '(if true 1) '()) 1)
  (check-equal? (eval-expr '(if false 1) '()) null)

  ; If, 2 arm
  (check-equal? (eval-expr '(if true 1 2) '()) 1)
  (check-equal? (eval-expr '(if false 1 2) '()) 2)

  ; Apply form
  (check-equal? (eval-expr '(car (quote (1 2 3))) (make-default-environment)) 1)

  ; Lambda test
  (check-equal? (eval-expr '((lambda (x) x) 1) (make-default-environment)) 1)
  (check-equal? (eval-expr '((lambda (x) (+ x x)) 4) (make-default-environment)) 8)
  (check-equal? (eval-expr '((lambda (x y)
                               ((lambda (a b)
                                  (+ (+ a a) b))
                                x y))
                             (+ 1 2) (+ 1 3 4))
                           (make-default-environment)) 14)
  )