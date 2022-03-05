
(defun find-capital (table country)
    (loop
    for i from 1 to 4
    do
	(cond ((null table) Nil)
		((eq (caar table) country) (return (cdar table)) )
		(T (setf table (cdr table)))
    ) )
)

(defun find-country (table capital)
	(cond ((null table) Nil)
		((eq (cdar table) capital) (caar table)) 
		(T (find-country (cdr table) capital))) )


(find-capital  '((Russia . Moscow) (Spain . Madrid) (France . Paris)) 'Paris)