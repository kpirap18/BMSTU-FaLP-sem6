(defun find-elem-in-set (set1 elem) 
	(cond ((null set1) Nil)
		((= (car set1) elem) T)
		(T (find-elem-in-set (cdr set1) elem)) ) )

(defun set-equal-rec (set1 set2) 
	(cond ((null set1))
		((find-elem-in-set set2 (car set1)) (set-equal-rec (cdr set1) set2))
		(T Nil)) )

(defun set-equal (set1 set2)
    (if (= (length set1) (length set2)) 
        (set-equal-rec set1 set2) Nil) )

(set-equal '(1 2 3) '(4 5 6)) ;; Nil
(set-equal '(1 2 3) '(3 1 2)) ;; T