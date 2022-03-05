;; Напишите функцию, которая добавляет к множеству двухэлементных списков новый
;; двухэлементный список, если его там нет.


;; СРАВНИТЬ 2 СПИСКА
(defun find-elem-in-set (set1 elem) 
	(cond ((null set1) Nil)
		((= (car set1) elem) T)
		(T (find-elem-in-set (cdr set1) elem)) 
    )
)

(defun set-equal-rec (set1 set2) 
	(cond ((null set1) Nil)
		((find-elem-in-set set2 (car set1)) (set-equal-rec (cdr set1) set2))
		(T Nil)
    ) 
)

;; Принимаю список из двухэлементных списков и сам двухэлементный список
;; и каждый элемент такого списка сравниваю со вторым
(defun set-equal (set1 set2)
	(cond ((null set1))
		((set-equal-rec set2 (car set1)) T)
		(T (set-equal (cdr set1) set2))
    )
)

;; просто переписанная append 
(DEFUN MY_APPEND (LST1 LST2)
    (cond ( (ATOM LST1) LST2 )
        (T ( CONS (CAR LST1) (MY_APPEND (CDR LST1) LST2) ))
    )
)

;; append специальных двухэлементоным списков
(defun my_append2 (lst1 lst2)
    (MY_APPEND lst1 (cons lst2 Nil))
)


(defun task7 (lst1 lst2)
    (if (set-equal lst1 lst2) "lst2 in lst1" (my_append2 lst1 lst2))
)