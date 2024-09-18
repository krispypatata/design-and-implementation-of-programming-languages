; helper function that will implement tail recursion
( define ( fact1 n acc ) ;the accumulator is acc
    (if (< n 1) 
        acc
        ( fact1 (- n 1) (* acc n ) ) ;no further computations after this call
    )
)

; main function that will set the initial value of the accumulator to 1
( define ( factorial n ) ( fact1 n 1) )