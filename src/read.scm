(define-module read
  (export sexprs->carrot-expr)
  (use srfi-1)
  (use util)
  (use carrot-classes)

  (define *function-heap* (make-hash-table 'eq?))
  (define *gen-function-type-heap* (make-hash-table 'eq?))
  (define *synonyms* (make-hash-table 'eq?))
  (define *generic-names* '())

  (define (synonym-definition? x)
    (and (pair? x) (eq? (car x) 'synonym)))

  (define (generic-definition? x)
    (and (pair? x) (eq? (car x) 'generic)))

  (define (type-var? x)
    (char-upper-case? (string-ref (symbol->string x) 0)))


  ;;[sexpr] -> <crt-expr>
  (define (sexprs->carrot-expr sexprs)
    ;;(= (name T1 T2 U) x y e)
    (cond [(null? sexprs)
           (list (ref *function-heap* 'main)
                 *function-heap*)]

          [(synonym-definition? (car sexprs))
           (register-synonym! (car sexprs) *synonyms*)
           (sexprs->carrot-expr (cdr sexprs))]

          [(generic-definition? (car sexprs))
           (hash-table-put!
            *gen-function-type-heap* (cadar sexprs)
            (parse-generic-function (car sexprs)))
           (set! *generic-names* (cons (cadar sexprs) *generic-names*))
           (sexprs->carrot-expr (cdr sexprs))]

          [else
           (register-function! (car sexprs) *function-heap*)
           (sexprs->carrot-expr (cdr sexprs))]))


  (define (parse-generic-function def)
    (let* ([name (cadr def)]
           [type-ins (butlast (cddr def))]
           [type-out (last def)])
      (make-generic-function name type-ins type-out)))

  (define (make-generic-function name type-ins type-out)
    (make <crt-generic-function>
      :name   name
      :expr   '()
      :params (map (lambda [_] (gensym "p")) (iota (length type-ins)))
      :type   (make <crt-generic-function-type>
                :arity (length type-ins)
                :param-types (map make-unknown-crt-type type-ins)
                :return-type (make-unknown-crt-type type-out))
      :methods '()))


  (define (register-function! def function-heap)
    (let* ([signature (cadr def)]
           [name      (car signature)]
           [params    (butlast (cddr def))]
           [expr      (last def)]
           [fn-type   (make-function-type (cons 'Fn (cdr signature)))]
           [fn-expr   (make <crt-function>
                        :name   name
                        :params params
                        :expr
                        (make-expr expr (zip params (get-param-types fn-type)) fn-type)
                        :type   fn-type)])

      (if (member name *generic-names*)
          (add-method! (ref *gen-function-type-heap* name) fn-expr)
          (hash-table-put! *function-heap* name fn-expr))))


  (define (add-method! gen-fn fn-expr)
    (let* ([methods  (get-methods gen-fn)]
           [fn-t     (get-type fn-expr)]
           [param-ts (get-param-types fn-t)])
      (set! (get-methods gen-fn)
            (acons param-ts fn-expr methods))))


  (define (synonym-expand t)
    (if (pair? t)
        (let1 alias (hash-table-get *synonyms* (car t) #f)
              (cond [(and alias (closure? alias))
                     (apply alias (cdr t))]
                    [alias alias]
                    [else t]))
        (let1 alias (hash-table-get *synonyms* t #f)
              (if alias alias t))))

  (define (flatten-signature signature)
    (let ([type-ins (map synonym-expand (butlast (cdr signature)))]
          [type-out (synonym-expand (last signature))])
      (if (and (pair? type-out) (eq? (car type-out) 'Fn))
          (flatten-signature (cons 'Fn (append type-ins (cdr type-out))))
          (cons 'Fn (append type-ins (list type-out))))))


  (define (make-function-type signature)
    (let* ([signature (flatten-signature signature)]
           [type-ins  (butlast (cdr signature))]
           [type-out  (last signature)])
      (make <crt-function-type>
        :arity (length type-ins)
        :param-types (map make-unknown-crt-type type-ins)
        :return-type (make-unknown-crt-type type-out))))


  (define (make-unknown-crt-type x)
    (case x
      [(String Str)
       (make <crt-string-type>)]
      [(Number Num)
       (make <crt-number-type>)]
      [(Character Char)
       (make <crt-char-type>)]
      [(Keyword Kw)
       (make <crt-keyword-type>)]
      [(Symbol Sym)
       (make <crt-symbol-type>)]
      [else
       (cond [(is-a? x <crt-type>) x]
             [(and (pair? x) (eq? 'Fn (car x)))
              (make-function-type x)]
             [(pair? x)
              (make <crt-composite-type>
                :container (car x)
                :content-types (map make-unknown-crt-type (cdr x)))]
             [else (make <crt-type-var> :id x)])]))


  (define (make-literal expr t)
    (make <crt-literal>
      :expr expr
      :type (make-unknown-crt-type t)))


  (define (make-expr expr params self-type)
    (cond
     [(is-a? expr <crt-expr>) expr]
     [(string? expr)  (make-literal expr 'String)]
     [(number? expr)  (make-literal expr 'Number)]
     [(char? expr)    (make-literal expr 'Char)]
     [(keyword? expr) (make-literal expr 'Keyword)]
     [(and (pair? expr) (pair? (cdr expr)) (eq? 'quote (cadr expr)))
      (make-literal (cadr expr) 'Symbol)]
     [(and (symbol? expr) (member expr (map car params)))
      (make <crt-local-ref>
        :expr expr
        :type (cadr (assq expr params)))]
     [(symbol? expr)
      (cond [(hash-table-get *function-heap* expr #f) => (^f f)]
            [(member expr *generic-names*)
             (ref *gen-function-type-heap* expr)]   ;;generic fn
            [else (make <crt-external-ref>
                    :expr expr
                    :type self-type)])]
     [(and (pair? expr) (eq? '^ (car expr)))
      (make-lambda expr params self-type)]
     [(pair? expr)
      (make-app-expr expr params self-type)]))


  (define (make-lambda expr env self-type)
    (let* ([params (butlast (cdr expr))]
           [param-ts (map (lambda [_] (make <crt-type-var>
                                        :id (gensym "tvar")))
                          params)]
           [expr (make-expr (last expr) (append (zip params param-ts) env) self-type)])
      (make <crt-function>
        :params params
        :expr   expr
        :type   (make <crt-function-type>
                  :param-types param-ts
                  :return-type (get-type expr)
                  :arity (length params)))))


  (define (select-method gen-fn args params self-type)
    (let* ([arity    (get-arity (get-type gen-fn))]
           [operand-ts
            (map (compose get-type (cut make-expr <> params self-type))
                 (take args arity))]
           [method   (assoc operand-ts (get-methods gen-fn))])
      (if method
          (make-expr (cons (cdr method) args) params self-type)
          (lambda [xs]
            ;;dispaatch on parametric type
            (select-method gen-fn xs params self-type)))))



  (define (make-app-expr expr params self-type)
    (let1 left (make-expr (car expr) params self-type)
          (cond
           [(is-a? left <crt-generic-function>)
            (select-method left (cdr expr) params self-type)]
           [(closure? (get-expr left))
            ((get-expr left) (cdr expr))]
           [(> (length expr) 1)
            (let* ([operator-expr (make-expr (butlast expr) params self-type)]
                   [operand-expr  (make-expr (last expr) params self-type)])
              (make <crt-app>
                :operator operator-expr
                :operand  operand-expr
                :expr     '() ;expr
                :type     (get-partial-return-type (get-type operator-expr) 1)))]
           [else left])))




  ;; synonym-statement * {synonyms} -> ()
  (define (register-synonym! synonym synonyms-ht)
    (let* ([alias  (cadr synonym)]
           [actual (caddr synonym)]
           [unique-actual (car (uniquify-type-var actual '()))])
      (hash-table-put! synonyms-ht alias unique-actual)))




  ;; (Container a b) -> (Container tvar12 tvar34)
  (define (uniquify-type-var t syms)
    (cond [(and (pair? t) (eq? 'lambda (car t)))
           (cons (eval t (interaction-environment)) syms)]  ;;polymorphic synonym
          [(pair? t)
           (cons (cons (car t) (car (fold (fn [t acc]
                                              (let1 x (uniquify-type-var t (cdr acc))
                                                    (cons (cons (car x) (car acc))
                                                          (cdr x))))
                                          '(() . ())
                                          (cdr t))))
                 syms)]
          [(type-var? t)
           (cons t syms)]
          [else
           (let1 s (assq t syms)
                 (if s (cons (cdr s) syms)
                     (let1 s- (gensym "tvar")
                           (cons s- (acons t s- syms)))))]))
  )
