; DESCRIPTION: This is a simple program that uses tail recursion to: 
; a. compute distances between pairs of coordinate points; and
; b. sort a list of numbers in random order, with those divisible by 5
; sorted in descending order first, followed by the remaining numbers, 
; also sorted in descending order (the result is a list with two 
; descendingly sorted groups).
; AUTHOR: Keith Ginoel S. Gabinete
; CREATED: September 20, 2024

; (computeDistance (list (cons -1 2) (cons 0 2) (cons 2 10)))
(define (computeDistanceTR pointsList totalDistance)
    (if (null? (cdr pointsList))  ; check if the next node/element is null
        totalDistance   ; base case
        
        (computeDistanceTR      ; recursive case
            (cdr pointsList)    ; updated list
            (+                  ; updated accumulator (totalDistance + (distance between the currently traversed point/node and the next point/node in the list))
                totalDistance 
                ; distance formula = sqrt ( (x1 - x2) ^ 2 + (y1 - y2) ^ 2 )
                (sqrt 
                    (+ 
                        (expt (- (caar pointsList) (caadr pointsList)) 2) 
                        (expt (- (cdar pointsList) (cdadr pointsList)) 2) 
                    ) 
                ) 
            ) 
        )
    )
)

; helper function
(define (computeDistance pointsList)
    (computeDistanceTR pointsList 0)
)



