(defun my-append(lst1 lst2)
	(cond ((null lst1)lst2)
		  ((cons (car lst1) (my-append (cdr lst1) lst2)))
	)
)


(defun my-reverse (lst)
	(cond ((null lst) Nil)
		  (t (my-append (my-reverse (cdr lst)) (list (car lst))))
	)
)

(defun my-equal (lst1 lst2)
	(cond ((and (atom lst1)(atom lst2))(eql lst1 lst2))
		  ((or (and (atom lst1) (not (atom lst2)))
		       (and (not (atom lst1)) (atom lst2))) Nil)
		  (t (and (my-equal (car lst1) (car lst2))
				  (my-equal (cdr lst1) (cdr lst2))))
	)
)

(defun is-palindrome (lst)
	(my-equal lst (my-reverse lst))
)

;---------------------

(defun is-palindrome-2 (lst)
	(equalp lst (reverse lst))
)


;----------------------



