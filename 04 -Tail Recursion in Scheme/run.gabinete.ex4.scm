(load "gabinete.ex4.scm")

(define sampleList (list (cons -1 2) (cons 0 2) (cons 2 10)))
(display sampleList)
(display "\n")
(display (car (cons -1 2)))
(display "\n")
(display (cdr (cons -1 2)))
(display "\n")

(display (computeDistance (list (cons -1 2) (cons 0 2) (cons 2 10))))
(display "\n")