

( define (list-sum list1 acc)
    (if ( null? list1 )
        acc ; base case
        ; (+ ( car list1 ) ( list-sum ( cdr list1 ) ) )
        (list-sum (cdr list1) (+ (car list1) acc)) ; recursive case
    )
)

; helper function
(define (sumList list2)
    (list-sum list2 0) ; call using initial value
)

; cons = alternative to append function in lists
; note: element should go first before the list
; example:
; (append `(5)'(1 2 3 4))
; (cons 5 '(1 2 3 4)) 
; returns (5 1 2 3 4)