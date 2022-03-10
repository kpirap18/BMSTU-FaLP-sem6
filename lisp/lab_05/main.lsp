; 1 палондром или нет 
(defun palindrome-p (list)
  (let* ((length (length list))
         (half-length (ceiling length 2))
         (tail (nthcdr half-length list))
         (reversed-head (nreverse (butlast list half-length))))
    (equal tail reversed-head)))

; 2 Написать предикат set-equal равны ли два множ или нет
(defun my-subsetp (set1 set2)
    (reduce
        #'(lambda (acc1 set1-el)
            (and acc1 (reduce
                #'(lambda (acc2 set2-el)
                    (or acc2 (= set2-el set1-el))) set2 :initial-value Nil)))
    set1 :initial-value T)
)

(defun set-equal (set1 set2)
    (if (= (length set1) (length set2))
        (and (my-subsetp set1 set2) (my-subsetp set2 set1))
        Nil)
)

; 3 страна.столица
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

; 4 swap-first-last,
(defun f1 (lst)
	(reverse (cdr (reverse lst))) )

(defun swap-first-last (lst)
	(append (append (last lst) (cdr (f1 lst))) (cons (car lst) Nil)) )

(defun swap-first-last2 (lst)
	(append (append (last lst) (butlast (cdr lst) 1)) (cons (car lst) Nil))) )


; 5 swap-two-ellement
(defun cut-list (lst n l) 
  (cond ((zerop l) nil)
        ((and (= n 1) (> l 0)) (cons (car lst) (cut-list (cdr lst) 1 (- l 1))))
        (t (cut-list (cdr lst) (- n 1) l))))
 
(defun task (lst p q)
  (cond ((= p q) lst)
        ((> p q) (task lst q p))
        (t  (let* ((ls (length lst))
                  (l (cut-list lst 1 (- p 1)))
                  (m (cut-list lst p (- q p)))
                  (r (cut-list lst q (- ls q -1))))
            (append l (list (car r)) (cdr m) (list (car m)) (cdr r))))))


; 6 swap-to-left и swap-to-right
(defun rot-left(n l)
  (append (nthcdr n l) (butlast l (- (length l) n))))

(defun rot-right(n l)
  (rot-left (- (length l) n) l))

; 9  select-between
(defun bubble_move (lst)
    (cond ((atom (cdr lst)) lst)
          ((> (car lst) (cadr lst)) (cons (cadr lst) (bubble_move (cons (car lst) (cddr lst)))))
          (T lst)   
    )
)
(defun my-sort (lst)
    (cond ((atom (cdr lst)) lst)
          (T (bubble_move (cons (car lst) (my-sort (cdr lst)))))
    )
)

;-------
(defun find-elements (lst left right)
	(remove-if #'(lambda (x) (null x))
		(mapcar #'(lambda (x) (if (< left x right) x)) lst))
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
