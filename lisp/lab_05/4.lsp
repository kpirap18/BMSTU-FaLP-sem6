

(defun find-capital (table country)
	(cond ((null table) Nil)
		((eq (caar table) country) (cdar table)) 
		(T (find-capital (cdr table) country))) )

(defun find-country (table capital)
	(cond ((null table) Nil)
		((eq (cdar table) capital) (caar table)) 
		(T (find-country (cdr table) capital))) )

(find-capital 
    '((Russia . Moscow)
    (Spain . Madrid)
    (France . Paris)) 'Russia) ;; MOSCOW


(defun find-country (table capital)
	(cond ((null table) Nil)
		((eq (cdar table) capital) (caar table)) 
		(T (find-country (cdr table) capital))) )

(find-capital 
    '((Russia . Moscow)
    (Spain . Madrid)
    (France . Paris)) 'Russia) ;; MOSCOW
