(add-load-path "." :relative)

(use carrot-classes)
(use read)
(use check)

(define (main args)
  (let* ([xs (sexprs->carrot-expr (read-from-string (cadr args)))]
         [main (car xs)]
         [heap (cadr xs)])
    (set-function-heap! heap)
    (format #t "Correctly Typed: ~S\n" (check-expr main '()))
    (print main)))
