; Prefix notation
(+ 1 2 3 4 5 6)

(* $2 6)

; Defining a variable
(define myvar 90)

#|
This is a multi-line comment

|#

; Boolean
; #t = true
; #f = false

;char-ci = case insensitive

; ,q = quit parent

; equal? = equivalence comparator

; eq? = identity comparator (stricter than equal?) (exactly the same memory slot) 

; strings and symbols are almost the same
; however, when you define a string with similar values, each string will take up a different memory slot
; on the otherhand, when you define symbols with alike values, they only take up a single memory slot

; NOTE: you can declare a variable named 'hello' and define a symbol 'hello

; # = create a vector
; vector = create a vector (alternative)

; vector-ref = get the element at certain index
; vector-set! = change the value of an element at certain index


; use 'cons' to define a pair
; car = access first element of the pair
; cdr = access second element
; set-car = change the value of the first element in the pair
; set-cdr = change the value of the second element
#|
scheme@(guile-user)> ( define pair1 ( cons 1 2) )
scheme@(guile-user)> (car pair1)
$30 = 1
scheme@(guile-user)> (cdr pair1)
$31 = 2
|#

; to define a list use the 'list' keyword
; alternative (use single quote ' before the first parenthesis)
#|
scheme@(guile-user)> mylist
$1 = (1 2 3 4)
|#

(if (> count 0) (set! count (- count 1)))


; function
(define (getVolume l w h)
    (* l * w h)
)

( define ( square n ) (* n n ))