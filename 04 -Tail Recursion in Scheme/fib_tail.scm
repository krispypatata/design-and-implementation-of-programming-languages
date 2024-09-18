; DESCRIPTION: Tail-recursive version of fibonacci
; AUTHOR: Keith Ginoel S. Gabinete
; CREATED: 9/18/2024

; fibT(n, prev, acc)
( define (fibT n prev acc)
    ( if (< n 2)
        acc ; base case
        
        (fibT (- n 1) acc (+ prev acc)) ; recursive case
        ; fibT(n - 1, acc, prev + acc)
    )
)

; Main

; Helper function
(define (fib n)
    (fibT n 0 1)
)

; (fibT 6 0 1) ; initial call without helper function