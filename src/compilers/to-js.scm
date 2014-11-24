(define-module to-js
  (export compile)
  (use carrot-classes)
  (use util)

  (define *function-heap* (make-hash-table 'eq?))

  (define (compile heap)
    (set! *function-heap* heap)
    (string-append
     (compile-expr (get-expr (ref heap 'main)))
     ";function _native_plus (x, y) { return x + y; }"))


  (define-method compile-expr [(expr <crt-function>)]
    (if (null? (get-params expr))
        (format "~A" (compile-expr (get-expr expr)))
        (format "function ~A (~A) { return ~A; }"
                (to-valid-id (get-name expr))
                (car (get-params expr))
                (compile-expr (get-partial-function expr)))))


  (define-method compile-expr [(expr <crt-app>)]
    (format "(~A)(~A)"
            (compile-expr (get-operator expr))
            (thunk (compile-expr (get-operand expr)))))

  (define-method compile-expr [(expr <crt-ffi>)]
    (format "~A(~A)"
            (get-operator expr)
            (fold (lambda [x str]
                    (string-append str (if (zero? (string-length str)) "" ",") x))
                  ""
                  (map compile-expr (get-operands expr)))))

  (define-method compile-expr [(expr <crt-local-ref>)]
    (format "~A()" (to-valid-id (get-expr expr))))

  (define-method compile-expr [(expr <crt-ref>)]
    (format "~A" (to-valid-id (get-expr expr))))

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
    (format "'~A'" x))

  (define-method compile-literal [x (_ <crt-any-type>)]
    (format "{}"))


  (define (thunk expr)
    (format "function () { return ~A; }" expr))


  (define (replace-incompatible-chars str)
    (let* ([str (regexp-replace-all #/-/ str "_")]
           [str (regexp-replace-all #/!/ str "_BANG_")]
           [str (regexp-replace-all #/\?/ str "_Q_")]
           [str (regexp-replace-all #/\*/ str "_STAR_")]
           [str (regexp-replace-all #/</ str "_LT_")]
           [str (regexp-replace-all #/>/ str "_GT_")]
           [str (regexp-replace-all #/\// str "_SLASH_")]
           [str (regexp-replace-all #/\+/ str "_SUM_")]
           [str (regexp-replace-all #/=/ str "_EQ_")]
           [str (regexp-replace-all #/%/ str "_PERC_")]
           [str (regexp-replace-all #/^false$/ str "_FALSE_")]
           [str (regexp-replace-all #/^true$/ str "_TRUE_")]
           [str (regexp-replace-all #/^if$/ str "_IF_")]
           [str (regexp-replace-all #/^delete$/ str "_DELETE_")])))


    (define (to-valid-id s)
      (replace-incompatible-chars (symbol->string s))))
