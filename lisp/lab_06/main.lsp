; лаба 6


; 1. Напишите функцию, которая уменьшает на 10 все числа из списка-аргумента этой функции
; одноуровневые список
(defun minus-d (lst)
	(mapcar #'(lambda (x) (- x 10))
			lst)
)

(defun f (lst res)
	(cond ((null lst) (reverse res))
	(T (f (cdr lst) (cons (- (car lst) 10) res))) ) )

(defun my-minus (lst)
    (f lst ()))

; структурированный список
(defun minus-d-all (lst)
	(mapcar #'(lambda (x) 
				(cond ((numberp x)(- x 10))
					  ((listp x)(minus-d-all x))
					  (t x)))
	lst)
)

(defun f (lst num)
	(cond ((null lst) ())
	((symbolp (car lst)) (cons (car lst) (f (cdr lst) num)))
	((listp (car lst)) (cons (f (car lst) num) (f (cdr lst) num)))
	(T (cons (- (car lst) 10) (f (cdr lst) num))) ) )
	
; 2. Напишите функцию, которая умножает на заданное число-аргумент все числа 
; из заданного списка-аргумента, когда 
; a) все элементы списка -- числа,
; только числа
(defun mult (lst n)
	(mapcar #'(lambda (x) (* x n))
			lst)
)
; одноур. список
(defun mult-els (lst num)
    (mapcar #'(lambda (arg)
        (cond ((numberp arg) (* arg num))
                (t arg))) lst)) 
; рекурсивно 
(defun mult-els-rec (lst num res)
    (cond 
        ((null lst) (reverse res))
        ((numberp (car lst)) (mult-els-rec (cdr lst) num (cons (* (car lst) num) res)))
        (t (mult-els-rec (cdr lst) num (cons (car lst) res)))))
(defun f (lst num)
    (mult-els-rec lst num ()))

; б) элементы списка -- любые объекты
(defun mult-all (lst n)
    (mapcar #'(lambda (x) 
            (cond ((numberp x)(* x n))
                ((listp x)(mult-all x n))
                (t x))) lst))

; рекурсия &&&&&&&&&&&&&
(defun mult-els-rec-deep (lst num)
    (cond
        ((null lst) nil)
        ((numberp (car lst)) (cons (* (car lst) num) (mult-els-rec-deep (cdr lst) num)))
        ((listp (car lst)) (cons (mult-els-rec-deep (car lst) num) (mult-els-rec-deep (cdr lst) num)))
        (t (cons (car lst) (mult-els-rec-deep (cdr lst) num)))))

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

; одноуровневый список
(defun get-sqr-list (lst)
    (cond 
        ((null lst) nil)
		((symbolp (car lst)) (cons (car lst) (get-sqr-list (cdr lst))))
        ((numberp (car lst)) (cons (* (car lst) (car lst)) (get-sqr-list (cdr lst))))
        (t (get-sqr-list (cdr lst)))))

; рекурсия без накопления cons, но с reverse
(defun get-sqr-list (lst res)
    (cond 
        ((null lst) (reverse res))
		((symbolp (car lst)) (get-sqr-list (cdr lst) (cons (car lst) res)))
        ((numberp (car lst)) (get-sqr-list (cdr lst) (cons (* (car lst) (car lst)) res)))))

(defun get-sqr (lst)
    (get-sqr-list (lst ())))
; c функционалами
(defun get-sqr-helper (el)
    (cond
        ((numberp el) (cons (* el el) nil))
		((symbolp el) (cons el nil))
        (t nil)))

(defun get-sqr-list-fun (lst)
    (mapcan #'get-sqr-helper lst))

; Рекурсивно для смешанного структурированного списка &&&&&&&&&&&&
(defun get-sqr-list (lst)
    (cond 
        ((null lst) nil)
		((symbolp (car lst)) (cons (car lst) (get-sqr-list (cdr lst))))
        ((listp (car lst)) (cons (get-sqr-list (car lst)) (get-sqr-list (cdr lst))))
        ((numberp (car lst)) (cons (* (car lst) (car lst)) (get-sqr-list (cdr lst))))
        (t (get-sqr-list (cdr lst)))))

; С использованием функционала для смешанного структурированного списка
(defun get-sqr-helper (el)
    (cond
        ((listp el) (cons (get-sqr-list-fun el) nil))
        ((numberp el) (cons (* el el) nil))
		((symbolp el) (cons el nil))
        (t nil)))

(defun get-sqr-list-fun (lst)
    (mapcan #'get-sqr-helper lst))

; 6. Напишите функцию, select-between, которая из списка-аргумента, содержащего только числа, 
; выбирает только те, которые расположены между двумя указанными границами-аргументами
; и возвращает их в виде списка (упорядоченного по возрастанию списка чисел (+ 2 балла)).

;
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


; (defun find-min (lst)
; 	(setf temp (car lst))
; 	(mapcar #'(lambda (x)
; 				(if (> temp x)
; 					(setf temp x))) lst)
; 	temp
; )

; (defun set-element (new old lst)
; 	(cond ((= (car lst) old) (rplaca lst new))
; 		  (t (set-element new old (cdr lst))))
; 	lst
; )

; (defun my-sort (lst)
; 	(maplist #'(lambda (x)
; 					(and (setf temp (find-min x))
; 						 (set-element (car x) temp x)
; 						 (rplaca x temp))) lst)		 
; 	lst
; )

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

; Рекурсивно. Для смешанного структурированного списка. &&&&&&&&&&&&
(defun select-rec (lst a b res)
    (cond
        ((null lst) res)
        ((listp (car lst)) (cons (select-rec (car lst) a b res)
                                 (select-rec (cdr lst) a b res)))
        ((and
            (numberp (car lst))
            (<= (car lst) b) 
            (>= (car lst) a)) 
            (select-rec (cdr lst) a b (cons (car lst) res)))
        (t (select-rec (cdr lst) a b res))))
; (select-rec '(1 2 (3 4 #'+ 3) ad 3 2 zxcv) 1 3)

; С использованием функционала. Для смешанного списка.
(defun select-fun-one-lvl (lst a b)
    (remove-if-not #'(lambda (el) (and (numberp el) (<= el b) (>= el a))) lst))

; С использованием функционала. Для смешанного структурированного списка.
(defun select-fun (lst a b)
    (mapcan #'(lambda (el) (cond
            ((listp el) (select-fun el a b))
            ((and (numberp el) (<= el b) (>= el a) (cons el nil))))) lst))

; обёрточная функция для каждой из предоставленной выше функции
(defun select-between (lst fNum sNum)
    (let ((a (cond ((< fNum sNum) fNum) (t sNum)))
          (b (cond ((< fNum sNum) sNum) (t fNum))))
          (select-rec lst a b ())))


; 7. Написать функцию, вычисляющую декартово произведение двух своих списковаргументов. 
; (Напомним, что А х В это множество всевозможных пар (a b), где а принадлежит А, принадлежит В.)

; функционалы
(defun decart (lstx lsty)
	(mapcan #'(lambda (x)
		(mapcar #'(lambda (y)
			(list x y)) lsty)) lstx)
)

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
