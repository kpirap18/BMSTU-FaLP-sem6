(defun my-last (lst)
	(cond ((null (cdr lst)) lst)
		  (t (my-last (cdr lst)))
	)
)

(DEFUN MY-APPEND (LST1 LST2)
    (cond ( (ATOM LST1) LST2 )
        (T ( CONS (CAR LST1) (MY-APPEND (CDR LST1) LST2) ))
    )
)


(defun find-capital (table country)
	(cond ((null table) Nil)
		((eq (caar table) country) (cdar table)) 
		((eq (caadr table) country) (cdadr table)) 
		((eq (caaddr table) country) (cdaddr table)) 
		((eq (caadddr table) country) (cdadddr table)) 
		(T Nil) ))

(defun find-country (table capital)
	(cond ((null table) Nil)
		((eql (cdar table) capital) (caar table)) 
		((eql (cdadr table) capital) (caadr table)) 
		((eql (cdaddr table) capital) (caaddr table)) 
		((eql (cdadddr table) capital) (caadddr table))  )

(find-capital 
    '((Russia . Moscow)
    (Spain . Madrid)
    (France . Paris)) 'Russia) ;; MOSCOW

(find-capital 
    '((Russia . Moscow)
    (Spain . Madrid)
    (France . Paris)) 'Russia) ;; MOSCOW


(assoc 'Russia '((Russia . Moscow) (Spain . Madrid) (France . Paris))) ;; MOSCOW
(rassoc 'Spain '((Russia . Moscow) (Spain . Madrid) (France . Paris))) ;; MOSCOW
; ----------------

; сразу возвращают страну или столицу
; столицу
(defun my-assoc (key table)
	(cond ((null table) nil)
		  ((if (equal key (caar table))
				(cdar table)
				(my-assoc key (cdr table))))
	)
)

; страну
(defun my-rassoc (val table)
	(cond ((null table) nil)
		  ((equal val (cdar table)) (caar table))
		  (T (my-rassoc val (cdr table)))
	)
)

; с помощью стандартных функций
(defun find-capital (key table)
	(cdr (assoc key table)))
	
(defun find-country (key table)
	(car (rassoc key table)))



;===============
(defun without-last (lst)
	(cond ((null (cdr lst)) nil)
		  ((null (cddr lst))(cons (car lst) Nil))
		  (t (my-append (cons (car lst) Nil) (without-last (cdr lst))))
	)
)

(defun swap-first-last (lst)
	(cond ((null (cdr lst)) lst)
		  (t (my-append (my-last lst) 
			      (my-append (without-last (cdr lst))
				  (cons (car lst) Nil))))
	)
)

