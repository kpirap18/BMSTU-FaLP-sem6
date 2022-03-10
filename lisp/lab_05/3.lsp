(defun my-equal (lst1 lst2)
	(cond ((and (atom lst1)(atom lst2))(eql lst1 lst2))
		  ((or (and (atom lst1) (not (atom lst2)))
		       (and (not (atom lst1)) (atom lst2))) Nil)
		  (t (and (my-equal (car lst1) (car lst2))
				  (my-equal (cdr lst1) (cdr lst2))))
	)
)

(defun find-capital (table country)
    (loop
    for i from 1 to 4
    do
	(cond ((null table) Nil)
		((my-equal (caar table) country) (return (cdar table)) )
		(T (setf table (cdr table)))
    ) )
)

(defun find-country (table capital)
	(cond ((null table) Nil)
		((eq (cdar table) capital) (caar table)) 
		(T (find-country (cdr table) capital))) )

(defun find-country (table capital)
    (loop
    for i from 1 to 4
    do
	(cond ((null table) Nil)
		((my-equal (cdar table) capital) (return (caar table)) )
		(T (setf table (cdr table)))
    ) )
)


(find-capital  '((Russia . Moscow) (Spain . Madrid) (France . Paris) (wewe . weer)) 'France)

(find-country  '((Russia . Moscow) (Spain . Madrid) (France . Paris) (wewe . weer)) 'Madrid)
