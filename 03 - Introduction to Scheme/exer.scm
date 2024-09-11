; DESCRIPTION: A collection of scheme commands for lab exercise for week 04.
; AUTHOR: Keith Ginoel S. Gabinete
; CREATED: September 11, 2024

; 1 (a)
; infix: 1 + (2 - 3) * 4 / 5 + (6 -7)
(+ ( + 1 ( / (* (- 2 3) 4) 5 ) ) (- 6 7))

; 1 (b)
; infix: (10 + 9) - 8 * 7 / (6 + 5) * 4 - 3
(- (- (+ 10 9) (* (/ (* 8 7) (+ 6 5) ) 4)) 3)

; 2 
; (define z (list 'a 'b (list 'c 'd) 'e))

; 2 (a) i.
; scheme@(guile-user)> (define z (list 'a 'b (list 'c 'd) 'e))
; scheme@(guile-user)> z
; $3 = (a b (c d) e)

; 2 (a) ii. Draw the linked list z.

; 2 (a) iii. command to display c.
; scheme@(guile-user)> (car (car (cdr (cdr z))))
; $8 = c

; 2 (b)
; (define z (list 'a 'b (list 'e (list 'f 'g ) ) (list (cons 'c 'd ) ) ) )

; 2 (b) i.
; scheme@(guile-user)> z
; $1 = (a b (e (f g)) ((c . d)))

; 2 (c) ii. Draw the linked list z.

; 2 (c) iii. Command to display c.
(car (car (car (cdr (cdr (cdr z)) ))))

; 3 (assume that the name of the list is 'subjects')
; 3 (a)
; (define z (list 127 (list 131 130) )  ) )
(define a (list 127 (list 131 130) (list 124 (list 141 (list 123)) 137 100) ) ) 
(car (car(cdr(cdr a))))

; 3 (b)
(define b (list 161 (list 100 (list 127 (list 125 (list 124 123)) 142))))
(car(car (cdr (car (cdr (car (cdr (car (cdr b)))))))))

; 3 (c)
(define c (list (list 141 123 128 (list 23 (cons (list ) 12) 24) 165) 22 21 ))
(car (cdr (cdr (car (cdr (cdr (cdr (car c))))))))