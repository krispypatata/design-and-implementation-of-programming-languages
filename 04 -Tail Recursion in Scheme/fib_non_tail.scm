; non-tail recursive function for fibonacci sequence

( define (fib n)
    (if (< n 2)
        n ; base case
        (+ (fib(- n 1)) (fib(- n 2))) ; recursive case
    )

)