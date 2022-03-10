(defun my-nth (n lst)
	(cond ((< n 0) nil)
		  ((= n 0)(car lst))
		  (t (my-nth (- n 1) (cdr lst)))
	)
)

(defun my-nsubst (new n lst)
	(cond ((= n 0)(setf (car lst) new))
		  (t (my-nsubst new (- n 1) (cdr lst)))
	)
)

(defun swap-two-elements (lst n1 n2)
	(let ((el1 (my-nth n1 lst))
		  (el2 (my-nth n2 lst)))
	(if (and el1 el2)
		(and	(my-nsubst el2 n1 lst)
				(my-nsubst el1 n2 lst)
				lst)
		(print "ERROR: wrong index")))


;------------------
(defun my-length-rec (lst n)
	(cond 
	((null lst) n)
	(T (my-length-rec (cdr lst) (+ n 1)))) )	

(defun my-length (lst)	
	(my-length-rec lst 0) )

(defun find-by-index-rec (lst index curr-index) 
	(cond ((null lst) Nil)
	((= index curr-index ) (car lst))
	(T (find-by-index-rec (cdr lst) index (+ curr-index 1)))) )

(defun find-by-index (lst index) 
	(find-by-index-rec lst index 0))

(defun swap-two-elements-rec (lst index1 index2 curr-index source-list res)
	(cond ((null lst) (reverse res))
		((= curr-index index1) (swap-two-elements-rec (cdr lst) index1 index2 (+ curr-index 1) source-list (cons (find-by-index source-list index2) res)))
		((= curr-index index2) (swap-two-elements-rec (cdr lst) index1 index2 (+ curr-index 1) source-list (cons (find-by-index source-list index1) res )))
		(T (swap-two-elements-rec (cdr lst) index1 index2 (+ curr-index 1) source-list (cons (car lst) res)) )) )

(defun swap-two-elements (lst i1 i2)
	(cond 
	((>= i1 (my-length lst)) "Первый индекс больше, чем размер списка")
	((>= i2 (my-length lst)) "Второй индекс больше, чем размер списка")
	((< i1 0) "Первый индекс меньше нуля")
	((< i2 0) "Второй индекс меньше нуля")
	((= i1 i2) lst)
	(T (swap-two-elements-rec lst i1 i2 0 lst ()))) )
