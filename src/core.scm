(add-load-path "." :relative)

(use carrot-classes)
(use read)

(define (main args)
  (display (sexprs->carrot-expr (read-from-string (cadr args)))))
