(define-module carrot-classes
  (export-all)
  (use srfi-9)
  (use util)

  (define-class <human-readable> () ())

  (define-method write-object [(x <human-readable>) out]
    (let* ([class (class-of x)]
           [serializable-slots (filter (^s (get-keyword :init-keyword (cdr s) #f))
                                       (class-slots class))]
           [slot-names (map car serializable-slots)]
           [init-kws (filter-map (^s (get-keyword :init-keyword (cdr s)))
                                 serializable-slots)])
      (if (null? init-kws)
          (format out "~A" (class-name class))
          (apply (pa$ format out
                      (str "(~A " (apply str (separate " ~A " init-kws)) " ~A)")
                      (class-name class))
                 (map (compose show (cut slot-ref x <>)) slot-names)))))



;;; Expression ;;;

  (define-class <crt-expr> (<human-readable>)
    ([expr :accessor get-expr
           :init-keyword :expr]
     [type :accessor get-type
           :init-keyword :type]))

  (define-class <crt-literal> (<crt-expr>) ())

  (define-class <crt-ref> (<crt-expr>) ())

  (define-class <crt-local-ref> (<crt-ref>) ())

  (define-class <crt-external-ref> (<crt-ref>) ())

  (define-class <crt-app> (<crt-expr>)
    ([operator :accessor get-operator
               :init-keyword :operator]
     [operand  :accessor get-operand
               :init-keyword :operand]))

  (define-class <crt-function> (<crt-expr>)
    ([name :accessor             get-name
           :init-value           'anonymous
           :init-keyword         :name]
     [params :accessor     get-params
             :init-value   '()
             :init-keyword :params]))

  (define-method get-partial-function [(f <crt-function>)]
    (if (= 1 (get-arity (get-type f)))
        (get-expr f)
        (make <crt-function>
          :expr (get-expr f)
          :name '--partial
          :params (cdr (get-params f))
          :type (get-partial-return-type (get-type f) 1))))

  (define-class <crt-generic-function> (<crt-function>)
    ([methods :accessor get-methods
              :init-keyword :methods
              :init-value '()]
     [env :init-value '()
          :accessor get-env]))



;;; Type ;;;

  (define-class <crt-type> (<human-readable>) ())

  (define-class <crt-primitive-type> (<crt-type>) ())
  (define-class <crt-string-type> (<crt-primitive-type>) ())
  (define-class <crt-number-type> (<crt-primitive-type>) ())
  (define-class <crt-char-type> (<crt-primitive-type>) ())
  (define-class <crt-keyword-type> (<crt-primitive-type>) ())
  (define-class <crt-symbol-type> (<crt-primitive-type>) ())

  (define-method object-equal? [(x <crt-primitive-type>) (y <crt-primitive-type>)]
    (eq? (class-of x) (class-of y)))

  (define-class <crt-function-type> (<crt-type>)
    ([arity :accessor            get-arity
            :init-keyword        :arity]
     [ret-t :accessor            get-return-type
            :init-keyword        :return-type]
     [param-ts :accessor         get-param-types
               :init-keyword     :param-types]
     [instanciator :accessor     get-instanciator
                   :init-value   (lambda [self . _] self)
                   :init-keyword :instanciator]))

  (define-class <crt-generic-function-type> (<crt-function-type>) ())

  (define-method object-equal? [(x <crt-function-type>) (y <crt-function-type>)]
    (and (equal? (get-return-type x) (get-return-type y))
         (equal? (get-param-types x) (get-param-types y))))

  (define-method get-partial-return-type [(ft <crt-function-type>) n]
    (if (= (get-arity ft) n)
        (get-return-type ft)
        (make <crt-function-type>
          :arity (- (get-arity ft) n)
          :return-type (get-return-type ft)
          :param-types (drop (get-param-types ft) n))))

  (define-class <crt-type-var> (<crt-type>)
    ([id :init-keyword :id
         :accessor get-symbol]))

  (define-method object-equal? [(x <crt-type-var>) (y <crt-type-var>)]
    (eq? (get-symbol x) (get-symbol y)))

  (define-class <crt-composite-type> (<crt-type>)
    ([container :accessor get-container-symbol
                :init-keyword :container]
     [content-types :accessor get-content-types
                    :init-keyword :content-types]))

  (define-method object-equal? [(x <crt-composite-type>) (y <crt-composite-type>)]
    (and (eq? (get-container-symbol x) (get-container-symbol y))
         (equal? (get-content-types x) (get-content-types y)))))
