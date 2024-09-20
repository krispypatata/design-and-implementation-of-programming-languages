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

(define list1 '(17 55 83 71 20 40 22 25 67 5))
(car list1)



(display (insertInSortedList 6 (list 8 7 5 4 )))
(display "\n")
(display (insertInSortedList 8 (list 8 7 5 4 )))
(display "\n")
(display (insertInSortedList 4 (list 8 7 5 4 )))
(display "\n")

(display (sortList (list 17 55 83 71 20 40 22 25 67 5)))
(display "\n")

(display (sortDivisible (list 17 55 83 71 20 83 40 22 25 5 67 5)))
(display "\n")