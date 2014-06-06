(define-module check
  (export check-expr set-function-heap!)
  (use srfi-1)
  (use util)
  (use carrot-classes)


  (define *function-heap* (make-hash-table 'eq?))

  (define (set-function-heap! heap)
    (set! *function-heap* heap))

  (define-method check-expr [(expr <crt-function>) binding]
    (let ([ret-t  (get-return-type (get-type expr))]
          [expr-t (get-type (get-expr expr))])
      (check-expr (get-expr expr) (append (unify ret-t expr-t binding) binding))))

  (define-method check-expr [(expr <crt-app>) binding]
    (let* ([operator (get-operator expr)]
           [operand  (get-operand  expr)]
           [binding  (append binding
                             (unify (car (get-param-types (get-type operator)))
                                    (get-type operand)
                                    binding))])
      (and
       (check-expr operator binding)
       (check-expr operand  binding))))

  (define-method prim-match? [(t <crt-number-type>) (u <number>)] #t)
  (define-method prim-match? [(t <crt-string-type>) (u <string>)] #t)
  (define-method prim-match? [(t <crt-symbol-type>) (u <symbol>)] #t)
  (define-method prim-match? [(t <crt-keyword-type>) (u <keyword>)] #t)
  (define-method prim-match? [(t <crt-char-type>) (u <char>)] #t)
  (define-method prim-match? [_ _] #f)


  (define-method check-expr [(expr <crt-literal>) _]
    (prim-match? (get-type expr) (get-expr expr)))

  (define-method check-expr [(expr <crt-local-ref>) _] #t)

  (define-method check-expr [(expr <crt-external-ref>) binding]
    (check-expr (ref *function-heap* (get-expr expr)) binding))




  ;;unify :: t u [(t-var . type)] -> [(t-var . type)]
  (define (unify t u binding)
    ;;(format #t "~S ≡ ~S\n" t u)
    (filter identity (unify- t u binding)))

  (define-method unify- [(t <crt-primitive-type>) (u <crt-primitive-type>) _]
    (if (eq? (class-of t) (class-of u)) '()
        (raise-error/message (format "~S -><- ~S" t u))))

  (define-method unify- [(t <crt-function-type>) (u <crt-function-type>) binding]
    (cons (unify (get-return-type t) (get-return-type u) binding)
          (apply append (map (lambda [x y] (unify x y binding))
                             (get-param-types t) (get-param-types u)))))

  (define-method unify- [(t <crt-type-var>) (u <crt-type-var>) binding]
    (let ([b1 (assoc t binding)]
          [b2 (assoc u binding)])
      (if (and b1 b2 (not (equal? (cdr b1) (cdr b2))))
          (raise-error/message "~S -><- ~S" (cdr b1) (cdr b2))
          '())))

  (define-method unify- [(t <crt-type-var>) u binding]
    (let1 b (assoc t binding)
          (if (and b (not (equal? (cdr b) u)))
              (raise-error/message (format "~S -><- ~S" (cdr b) u))
              (list (cons t u)))))

  (define-method unify- [t (u <crt-type-var>) binding]
    (let1 b (assoc u binding)
          (if (and b (not (equal? (cdr b) t)))
              (raise-error/message (format "~S -><- ~S" (cdr b) t))
              (list (cons u t)))))

  (define-method unify- [t u binding]
    (raise-error/message (format "~S -><- ~S" t u))))
