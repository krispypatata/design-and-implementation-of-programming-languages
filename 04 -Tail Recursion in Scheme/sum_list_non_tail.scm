; 

( define (list-sum list1 )
    (if (null? list1 )
        0
        (+ ( car list1 ) ( list-sum ( cdr list1 ) ) )
    )
)