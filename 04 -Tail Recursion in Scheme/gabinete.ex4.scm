; DESCRIPTION: This is a simple program that uses tail recursion to: 
; a. compute distances between pairs of coordinate points; and
; b. sort a list of numbers in random order, with those divisible by 5
; sorted in descending order first, followed by the remaining numbers, 
; also sorted in descending order (the result is a list with two 
; descendingly sorted groups).
; AUTHOR: Keith Ginoel S. Gabinete
; CREATED: September 20, 2024

; ═════════════════════════════════════════════════════════════════════════════════════
; PART 1: Compute Distance
(define (computeDistanceTR pointsList totalDistance)
    (if (null? (cdr pointsList))  ; Check if the next node/element is null
        totalDistance   ; Base case
        
        (computeDistanceTR      ; Recursive case
            (cdr pointsList)    ; Updated list
            (+                  ; Updated accumulator (totalDistance + (distance between the currently traversed point/node and the next point/node in the list))
                totalDistance 
                ; Distance formula = sqrt ( (x1 - x2) ^ 2 + (y1 - y2) ^ 2 )
                (sqrt 
                    (+ 
                        (expt (- (caar pointsList) (caadr pointsList)) 2) 
                        (expt (- (cdar pointsList) (cdadr pointsList)) 2) 
                    ) ; End - addition of squares
                ) ; End - sqrt
            ) ; End - addition of distances
        ) ; End - computeDistanceTR
    ) ; End - if
)

; -------------------------------------------------------------------------------------
; Helper function
(define (computeDistance pointsList)
    (computeDistanceTR pointsList 0)
)

; ═════════════════════════════════════════════════════════════════════════════════════
; PART 2: Sort Divisible by 5

; =====================================================================================
; The list parameter here must be already sorted in DESCENDING order.
(define (insertInSortedListTR elementToInsert sortedListLeft sortedListRight )
    (if (or (null? sortedListLeft) (> elementToInsert (car sortedListLeft)) )
        ; Base case
        ; Combine the two sublists
        (append sortedListRight (cons elementToInsert sortedListLeft))  

        ; Recursive case
        ; first param: the element to insert
        ; second param: updated sortedListLeft without the currently traversed node/element
        ; third param: sortedListRight with the currently traversed element added to it (since it will be removed in the next iteration)
        (insertInSortedListTR elementToInsert (cdr sortedListLeft) (append sortedListRight (list (car sortedListLeft))))
    )
)

; -------------------------------------------------------------------------------------
; Helper function
(define (insertInSortedList elementToInsert sortedList)
    (insertInSortedListTR elementToInsert sortedList (list ))
)

; =====================================================================================
; Sort a list in DESCENDING order with the insertInSortedList function
(define (sortListTR numList sortedNumList)
    (if (null? numList)
        sortedNumList   ; Base case

        ; Recursive Case
        ; firs param: updated numList without the currently traversed node/element
        ; second param: updatedSortedNumList with the current element inserted into it
        (sortListTR (cdr numList) (insertInSortedList (car numList) sortedNumList)) 
    )
)

; -------------------------------------------------------------------------------------
; Helper function
(define (sortList numList)
    (sortListTR numList (list ))
)

; =====================================================================================
(define (sortDivisibleTR numList sortedNumList)
    (if (null? numList) ; Check if current node/element is null
        ; Base case
        ; Sort the first and second sublist then combine them
        (append (sortList (car sortedNumList)) (sortList (cdr sortedNumList))) 

        ; Recursive case 
        ; first param: updated numList without the currently traversed node/element
        ; second param: updated sortedNumList with the current element in it (uses ternary operator to determine which side the current element should be appended to))
        (sortDivisibleTR (cdr numList)
            (if (= (modulo (car numList) 5) 0)  ; Check if the current node/element is divisible by 5
                (cons (cons (car numList) (car sortedNumList)) (cdr sortedNumList)) ; add node/element to the first sublist
                (cons (car sortedNumList) (cons (car numList) (cdr sortedNumList))) ; add node/element to the second sublist
            )
        )
    )  
)

; -------------------------------------------------------------------------------------
; Helper function
(define (sortDivisible numList)
    (sortDivisibleTR numList (cons (list ) (list )))
)
; ═════════════════════════════════════════════════════════════════════════════════════

