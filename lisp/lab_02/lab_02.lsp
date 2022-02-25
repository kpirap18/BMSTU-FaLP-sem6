;#1
(equal 3 (abs - 3)) 
(equal (* 2 3) (+ 7 2))
(equal (+ 1 2) 3) 
(equal (- 7 3) (* 3 2))
(equal (* 4 7) 21) 
(equal (abs (- 2 4)) 3))

;#2
; гипотенуза
(defun f (a b)
	(sqrt (+ (* a a) (* b b))))

;#3
; объем параллелепипеда
(defun v (a b c)
	(* a b c))

;#4
(list 'a c) 
(cons 'a 'b 'c)
(cons 'a (b c)) 
(list 'a (b c))
(cons 'a '(b c)) 
(list a '(b c))
(caddr (1 2 3 4 5)) 
(list (+ 1 '(length '(1 2 3))))

;#5
(defun l_t (ar1 ar2)
	(cond ((null ar2) (null (null ar1)))
			(T (l_t (cdr ar1) (cdr ar2)))
	)
)

(defun l_t (ar1 ar2)
	(> (length ar1) (length ar2))
)

;#6
(cons 3 (list 5 6)) 
(cons 3 '(list 5 6))
(list 3 'from 9 'lives (- 9 3))
(+ (length for 2 too)) (car '(21 22 23)))
(cdr '(cons is short for ans))
(car (list one two)) 
(car (list 'one 'two))


;#7
(defun mystery (x) (list (second x) (first x)))

(mystery (one two)) 
(mystery one 'two))
(mystery (last one two))
(mystery free)


;#8
(defun f-to-c (temp)
	(* 5/9 (- temp 32.0)))

;#9
(list 'cons t NIL) 
(eval (list 'cons t NIL))
(eval (eval (list 'cons t NIL)))
(apply #cons '(t NIL)) 
(eval NIL)
(list 'eval NIL) 
(eval (list 'eval NIL))


;#10
; катек по гипотенузе и катету
(defun f-c (a c)
	(sqrt (- (* c c) (* a a))))


;#11
; площадь трапеции 
(defun sq (a b h)
	(* 1/2 (+ a b) h))
