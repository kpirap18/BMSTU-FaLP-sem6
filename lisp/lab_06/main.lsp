; лаба 6


; 1. Напишите функцию, которая уменьшает на 10 все числа из списка-аргумента этой функции
; одноуровневые список
; только числа
(defun minus-d (lst)
	(mapcar #'(lambda (x) (- x 10)) lst))

; смешанный список
(defun minus-d (lst)
	(mapcar #'(lambda (x) (cond ((numberp x) (- x 10)) (x))) lst))

; рекурсия
; только числа
(defun f (lst res)
	(cond ((null lst) (reverse res))
	(T (f (cdr lst) (cons (- (car lst) 10) res))) ) )

; смешанный список
(defun f (lst res)
	(cond ((null lst) (reverse res))
          ((numberp (car lst)) (f (cdr lst) (cons (- (car lst) 10) res)))
	(T (f (cdr lst) (cons (car lst) res))) ) )

(defun my-minus (lst)
    (f lst ()))

; структурированный список
; вспомогательная функция
(defun dop-fun (lst)
    (cond ((and (numberp (car lst)) (numberp (cdr lst))) (cons (- (car lst) 10) (- (cdr lst) 10)))
          ((and (symbolp (car lst)) (numberp (cdr lst))) (cons (car lst) (- (cdr lst) 10)))
          ((and (numberp (car lst)) (symbolp (cdr lst))) (cons (- (car lst) 10) (cdr lst)))
          ((and (atom (cdar lst)) (numberp (cdr lst))) (cons (dop-fun (car lst)) (- (cdr lst) 10)))
          ((and (atom (cdar lst)) (symbolp (cdr lst))) (cons (dop-fun (car lst)) (cdr lst)))
          (T lst)))

; функционал
(defun minus-d-all (lst)
	(mapcar #'(lambda (x) 
				(cond ((numberp x) (- x 10))
                      ((atom x) x)
                      ((atom (cdr x)) (dop-fun x))
					  ((listp x) (minus-d-all x)))) lst))
; рекурсия
(defun f (lst)
	(cond ((null lst) ())
	((symbolp (car lst)) (cons (car lst) (f (cdr lst))))
	((numberp (car lst)) (cons (- (car lst) 10) (f (cdr lst))))
	((atom (car lst)) (cons (car lst) (f (cdr lst) )))
    ((atom (cdr lst)) (cons (dop-fun (car lst)) (f (cdr lst) )))
	(T (cons (f (car lst)) (f (cdr lst))))) )

; "хвостовая" рекурсия
(defun f (lst res)
	(cond ((null lst) (reverse res))
	    ((symbolp (car lst)) (f (cdr lst) (cons (car lst) res)))
	    ((numberp (car lst)) (f (cdr lst) (cons (- (car lst) 10) res)))
        ((atom (car lst)) (f (cdr lst) (cons (car lst) res)))
        ((atom (cdr lst)) (f (cdr lst) (cons (dop-fun (car lst)) res)))
	    (T (f (cdr lst) (cons (f (csr lst) ()) res))) ) )
	
; 2. Напишите функцию, которая умножает на заданное число-аргумент все числа 
; из заданного списка-аргумента, когда 
; a) все элементы списка -- числа,
; одноуровневые список
; только числа
(defun mult-f (lst num)
	(mapcar #'(lambda (x) (* x num)) lst))

; смешанный список
(defun mult-f (lst num)
	(mapcar #'(lambda (x) (cond ((numberp x) (* x num)) (x))) lst))

; рекурсия
; только числа
(defun f (lst num res)
	(cond ((null lst) (reverse res))
	(T (f (cdr lst) (cons (* (car lst) num) res))) ) )

; смешанный список
(defun f (lst num res)
	(cond ((null lst) (reverse res))
          ((numberp (car lst)) (f (cdr lst) (cons (* (car lst) num) res)))
	      (T (f (cdr lst) (cons (car lst) res))) ) )

(defun my-mult (lst num)
    (f lst num ()))

; структурированный список
; вспомогательная функция
(defun dop-fun (lst num)
    (cond ((and (numberp (car lst)) (numberp (cdr lst))) (cons (* (car lst) num) (* (cdr lst) num)))
          ((and (symbolp (car lst)) (numberp (cdr lst))) (cons (car lst) (* (cdr lst) num)))
          ((and (numberp (car lst)) (symbolp (cdr lst))) (cons (* (car lst) num) (cdr lst)))
          ((and (atom (cdar lst)) (numberp (cdr lst))) (cons (dop-fun (car lst)) (* (cdr lst) num)))
          ((and (atom (cdar lst)) (symbolp (cdr lst))) (cons (dop-fun (car lst)) (cdr lst)))
          (T lst)))

; функционал
(defun mult-d-all (lst num)
	(mapcar #'(lambda (x) 
				(cond ((numberp x) (* x num))
                      ((atom x) x)
                      ((atom (cdr x)) (dop-fun x))
					  ((listp x) (minus-d-all x)))) lst))
; рекурсия
(defun f (lst)
	(cond ((null lst) ())
	((symbolp (car lst)) (cons (car lst) (f (cdr lst))))
	((numberp (car lst)) (cons (* (car lst) num) (f (cdr lst))))
	((atom (car lst)) (cons (car lst) (f (cdr lst) )))
    ((atom (cdr lst)) (cons (dop-fun (car lst)) (f (cdr lst) )))
	(T (cons (f (car lst)) (f (cdr lst))))) )

; "хвостовая" рекурсия
(defun f (lst res)
	(cond ((null lst) (reverse res))
	    ((symbolp (car lst)) (f (cdr lst) (cons (car lst) res)))
	    ((numberp (car lst)) (f (cdr lst) (cons (* (car lst) num) res)))
        ((atom (car lst)) (f (cdr lst) (cons (car lst) res)))
        ((atom (cdr lst)) (f (cdr lst) (cons (dop-fun (car lst)) res)))
	    (T (f (cdr lst) (cons (f (csr lst) ()) res))) ) )

    
; 3. Написать функцию, которая по своему списку-аргументу lst определяет 
; является ли он палиндромом (то есть равны ли lst и (reverse lst)).
(defun palindrome-p (list)
  (let* ((length (length list))
         (half-length (ceiling length 2))
         (tail (nthcdr half-length list))
         (reversed-head (nreverse (butlast list half-length))))
    (equal tail reversed-head)))

(defun is-palindrome-2 (lst)
	(equalp lst (reverse lst))
)

; 4. Написать предикат set-equal, который возвращает t, если два его множества-аргумента
; содержат одни и те же элементы, порядок которых не имеет значения
; для одноуровнего списка
; функционал 
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

; рекурсия 
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

; 5. Написать функцию которая получает как аргумент список чисел, а возвращает 
; список квадратов этих чисел в том же порядке.
; одноуровневые список
; только числа
(defun mult-f (lst)
	(mapcar #'(lambda (x) (* x x)) lst))

; смешанный список
(defun mult-f (lst)
	(mapcar #'(lambda (x) (cond ((numberp x) (* x x)) (x))) lst))

; рекурсия
; только числа
(defun f (lst res)
	(cond ((null lst) (reverse res))
	(T (f (cdr lst) (cons (* (car lst) (car lst)) res))) ) )

; смешанный список
(defun f (lst res)
	(cond ((null lst) (reverse res))
          ((numberp (car lst)) (f (cdr lst) (cons (* (car lst) (car lst)) res)))
	      (T (f (cdr lst) (cons (car lst) res))) ) )

(defun my-mult (lst num)
    (f lst num ()))

; структурированный список
; вспомогательная функция
(defun dop-fun (lst)
    (cond ((and (numberp (car lst)) (numberp (cdr lst))) (cons (* (car lst) (car lst)) (* (cdr lst) (car lst))))
          ((and (symbolp (car lst)) (numberp (cdr lst))) (cons (car lst) (* (cdr lst) (car lst))))
          ((and (numberp (car lst)) (symbolp (cdr lst))) (cons (* (car lst) (car lst)) (cdr lst)))
          ((and (atom (cdar lst)) (numberp (cdr lst))) (cons (dop-fun (car lst)) (* (cdr lst) (car lst))))
          ((and (atom (cdar lst)) (symbolp (cdr lst))) (cons (dop-fun (car lst)) (cdr lst)))
          (T lst)))

; функционал
(defun mult-d-all (lst)
	(mapcar #'(lambda (x) 
				(cond ((numberp x) (* x x))
                      ((atom x) x)
                      ((atom (cdr x)) (dop-fun x))
					  ((listp x) (minus-d-all x)))) lst))
; рекурсия
(defun f (lst)
	(cond ((null lst) ())
	((symbolp (car lst)) (cons (car lst) (f (cdr lst))))
	((numberp (car lst)) (cons (* (car lst) (car lst)) (f (cdr lst))))
	((atom (car lst)) (cons (car lst) (f (cdr lst) )))
    ((atom (cdr lst)) (cons (dop-fun (car lst)) (f (cdr lst) )))
	(T (cons (f (car lst)) (f (cdr lst))))) )

; "хвостовая" рекурсия
(defun f (lst res)
	(cond ((null lst) (reverse res))
	    ((symbolp (car lst)) (f (cdr lst) (cons (car lst) res)))
	    ((numberp (car lst)) (f (cdr lst) (cons (* (car lst) (car lst)) res)))
        ((atom (car lst)) (f (cdr lst) (cons (car lst) res)))
        ((atom (cdr lst)) (f (cdr lst) (cons (dop-fun (car lst)) res)))
	    (T (f (cdr lst) (cons (f (csr lst) ()) res))) ) )


; 6. Напишите функцию, select-between, которая из списка-аргумента, содержащего только числа, 
; выбирает только те, которые расположены между двумя указанными границами-аргументами
; и возвращает их в виде списка (упорядоченного по возрастанию списка чисел (+ 2 балла)).

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

; -------
(defun find-elements (lst left right)
	(remove-if #'(lambda (x) (null x))
		(mapcar #'(lambda (x) (if (< left x right) x)) lst))
)

(defun select-between (lst b1 b2)
	(cond ((null lst) nil)
		  ((not (and (numberp b1) (numberp b2))) nil)
		  ((= b1 b2)(and (print "ERROR: wrong format of borders (equal)") nil))
		  ((> b1 b2)(my-sort (find-elements lst b2 b1)))
		  ((> b2 b1)(my-sort (find-elements lst b1 b2))))
)
;--------------------
; Рекурсивно. Для смешанного списка.
(defun select-rec-one-lvl (lst a b res)
    (cond 
        ((null lst) res)
        ((and 
            (numberp (car lst)) 
            (<= (car lst) b) 
            (>= (car lst) a)) 
            (select-rec-one-lvl (cdr lst) a b (cons (car lst) res)))
        (t (select-rec-one-lvl (cdr lst) a b res))))
(defun select-between (lst)
    (select-rec lst 1 10 ()))


; 7. Написать функцию, вычисляющую декартово произведение двух своих списковаргументов. 
; (Напомним, что А х В это множество всевозможных пар (a b), где а принадлежит А, принадлежит В.)

; функционалы
(defun decart (lstx lsty)
	(mapcan #'(lambda (x)
		(mapcar #'(lambda (y)
			(list x y)) lsty)) lstx))

; рекурсия без накапливаний cons
(defun decart-rec (el lst2 res)
    (cond ((null lst2) res)
          (t (decart-rec el (cdr lst2) (cons (cons el (cons (car lst2) Nil)) res) ))))

(defun decart (lst1 lst2 res)
    (cond ((null lst1) res)
          (t (decart (cdr lst1) lst2 (decart-rec (car lst1) lst2 res)))))

; просто рекурсия 
(defun decart-elem (lst elem)
	(cond ((null lst) ())
	(T (cons (list elem (car lst)) (decart-elem (cdr lst) elem)))) )

(defun decart (lst1 lst2)
	(cond ((null lst1) nil)
	(T (append (decart-elem lst2 (car lst1)) (decart (cdr lst1) lst2)))) )



; 8. Почему так реализовано reduce, в чем причина? 
; (reduce #'+ 0) -> 0 
; (reduce #'+ ()) -> 0


; 9. Пусть list-of-list список, состоящий из списков. Написать функцию, которая 
; вычисляет сумму длин всех элементов list-of-list, т.е. например для аргумента  
; ((1 2) (3 4)) -> 4.

; функционал
(defun list-of-list (lst)
	(reduce #'(lambda (acc lst) (+ acc (length lst)))
		(cons 0 lst)))
		
(defun my-length-rec (lst n)
	(cond 
	((null lst) n)
	(T (my-length-rec (cdr lst) (+ n 1)))) )	

(defun my-length (lst)	
	(my-length-rec lst 0) )

(defun list-of-list-rec (lst len)
	(cond ((null lst) len)
		  ((atom (car lst)) (list-of-list-rec (cdr lst) (+ len 1)))
		  ((and (atom (caar lst)) (atom (cdar lst))) (list-of-list-rec (cdr lst) (+ len 2)))
		  (T (list-of-list-rec (cdr lst) (+ len (my-length (car lst)) )))))

(defun list-of-list (lst)
	(list-of-list-rec lst 0))
