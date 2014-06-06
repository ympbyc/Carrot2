(define-module to-js
  (export compile)
  (use carrot-classes)
  (use util)

  (define *function-heap* (make-hash-table 'eq?))

  (define (compile heap)
    (set! *function-heap* heap)
    (compile-expr (get-expr (ref heap 'main))))


  (define-method compile-expr [(expr <crt-function>)]
    (format "function ~A (~A) { return ~A; }"
            (get-name expr)
            (car (get-params expr))
            (compile-expr (get-partial-function expr))))


  (define-method compile-expr [(expr <crt-app>)]
    (format "(~A)(~A)"
            (compile-expr (get-operator expr))
            (compile-expr (get-operand expr))))

  (define-method compile-expr [(expr <crt-ref>)]
    (format "~A" (get-expr expr)))

  (define-method compile-expr [(expr <crt-literal>)]
    (compile-literal (get-expr expr) (get-type expr)))



  (define-method compile-literal [x (_ <crt-string-type>)]
    (format "'~A'" x))

  (define-method compile-literal [x (_ <crt-number-type>)]
    (format "~D" x))

  (define-method compile-literal [x (_ <crt-symbol-type>)]
    (raise-error/message "js-compiler doesn't implement symbols"))

  (define-method compile-literal [x (_ <crt-keyword-type>)]
    (format "':~A'" x))

  (define-method compile-literal [x (_ <crt-char-type>)]
    (format "'~A'" x)))
