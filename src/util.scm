(define-module util
  (export-all)
  (use srfi-1)
  (use srfi-9)

  (define (butlast xs) (drop-right xs 1))

  (define (str . xs)
    (apply string-append (map show xs)))


  (define-method show [(x <string>)] x)
  (define-method show [(x <keyword>)] (string-append ":" (keyword->string x)))
  (define-method show [x] (format "~S" x))

  (define (separate x xs)
    (if (null? xs)
        '()
        (let1 tail (separate x (cdr xs))
              (cons (car xs)
                    (if (null? tail) tail (cons x tail))))))

  (define (p x)
    (print x)
    x)


  )
