;; Напишите функцию, select-between, которая из списка-аргумента из 5 чисел выбирает
;; только те, которые расположены между двумя указанными границами-аргументами и
;; возвращает их в виде списка (упорядоченного по возрастанию списка чисел (+ 2 балла))


(defun find-elements (lst left right)
	(remove-if #'(lambda (x) (null x))
					(mapcar #'(lambda (x)
								(if (< left x right)
									x)) lst))
)


(defun find-min (lst)
	(setf temp (car lst))
	(mapcar #'(lambda (x)
				(if (> temp x)
					(setf temp x))) lst)
	temp
)

(defun set-element (new old lst)
	(cond ((= (car lst) old) (rplaca lst new))
		  (t (set-element new old (cdr lst))))
	lst
)


(defun my-sort (lst)
	(maplist #'(lambda (x)
					(and (setf temp (find-min x))
						 (set-element (car x) temp x)
						 (rplaca x temp))) lst)
						 
	lst
)

(defun select-between (lst b1 b2)
	(cond ((null lst) nil)
		  ((not (and (numberp b1) (numberp b2)))(and (print "ERROR: wrong format of borders") nil))
		  ((= b1 b2)(and (print "ERROR: wrong format of borders (equal)") nil))
		  ((> b1 b2)(my-sort (find-elements lst b2 b1)))
		  ((> b2 b1)(my-sort (find-elements lst b1 b2))))
)