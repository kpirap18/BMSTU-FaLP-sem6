;#1
(defun f (num)
	(if (evenp num) num (+ num 1)))
	
;#2
(defun f(num)
	(if (< num 0) (- num 1) (+ num 1)))
	
;#3
(defun f (a b)
	(cond ((< ab) (list a b))
			(T (list b a))
	)
)

;#4
(defun f (a b c)
	(cond ((and (< b a) (< a c)) T)
		  ((and (< c a) (< a b)) T)))


;#5
(and 'fee 'fie 'foe) 
(or 'fee 'fie 'foe)
(or nil 'fie 'foe) 
(and nil 'fie 'foe)
(and (equal 'abc 'abc) 'yes) 
(or (equal 'abc 'abc) 'yes)

;#6
(defun fp (a b)
	(>= a b))

;#7

;#8

