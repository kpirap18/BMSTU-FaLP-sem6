; 1 Написать хвостовую рекурсивную функцию my-reverse, 
; которая развернет верхний уровень своего списка-аргумента lst
(defun move-to (lst res)
    (cond ((null lst) res)
          (T (move-to (cdr lst) (cons (car lst) res)))))

(defun my-reverse (lst)
    (move-to lst ()))

; 3 Написать функцию, которая возвращает первый 
;элемент списка -аргумента, который сам является непустым списком.

; рекурсия
(defun ret-first-lst (lst)
    (cond ((null lst) lst)
          ((and (listp (car lst)) (not (null (car lst)))) (car lst))
          (t (ret-first-lst (cdr lst)))))

; функционалы
(defun ret-first-lst-fun (lst)
    (find-if #'(lambda (x) (and (listp x) (not (null x)))) lst))

; 4 Написать функцию, которая выбирает из заданного 
; списка только те числа, которые больше 1 и меньше 10. 
; (Вариант: между двумя заданными границами. )
; рекурсивно для од. списка
(defun select-rec-one-lvl (lst a b res)
    (cond 
        ((null lst) res)
        ((and 
            (numberp (car lst)) 
            (<= (car lst) b) 
            (>= (car lst) a)) 
            (select-rec-one-lvl (cdr lst) a b (cons (car lst) res)))
        (t (select-rec-one-lvl (cdr lst) a b res))))

; Рекурсивно. Для смешанного структурированного списка.
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

; С использованием функционала. Для смешанного списка.
(defun select-fun-one-lvl (lst a b)
    (remove-if-not #'(lambda (el) (and (numberp el) (<= el b) (>= el a))) lst))

; С использованием функционала. Для смешанного структурированного списка.
(defun select-fun (lst a b)
    (mapcan #'(lambda (el) 
        (cond
            ((listp el) (select-fun el a b))
            ((and (numberp el) (<= el b) (>= el a) (cons el nil))))) lst))

; обёрточная функция для каждой из предоставленной выше функции
(defun select-between (lst)
    (select-rec lst 1 10 ()))

; +7  Напишите рекурсивную функцию, которая умножает на заданное 
; число-аргумент все числа из заданного списка-аргумента, когда
; С использованием функционала для одномерного смешанного списка
(defun mult-els (lst num)
    (mapcar #'(lambda (arg) (cond 
                ((numberp arg) (* arg num))
                (t arg))) 
        lst))

; С использованием функционала для структурированного смешанного списка
(defun mult-els-deep (lst num)
    (mapcar #'(lambda (arg) 
            (cond 
                ((listp arg) (mult-els-deep arg num))
                ((numberp arg) (* arg num))
                (t arg))) lst))

; Рекурсивно для одномерного смешанного списка
(defun mult-els-rec (lst num res)
    (cond 
        ((null lst) (reverse res)
        ((numberp (car lst)) (mult-els-rec (cdr lst) num (cons (* (car lst) num) res )))
        (t (mult-els-rec (cdr lst) num (cons (car lst) res)))))

(defun mult-els (lst num)
    (mult-els-rec (lst num ())))

; Рекурсивно для структурированного смешанного  списка &&&&&&&&&
(defun mult-els-rec-deep (lst num)
    (cond
        ((null lst) nil)
        ((listp (car lst)) (cons (mult-els-rec-deep (car lst) num) (mult-els-rec-deep (cdr lst) num)))
        ((numberp (car lst)) (cons (* (car lst) num) (mult-els-rec-deep (cdr lst) num)))
        (t (cons (car lst) (mult-els-rec-deep (cdr lst) num)))))

; 8  Напишите функцию, select-between, которая из списка-аргумента, 
; содержащего только числа, выбирает только те, которые расположены
; между двумя указанными границамиаргументами и возвращает их в виде
; списка (упорядоченного по возрастанию списка чисел (+ 2 балла)).

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

; 8 Написать рекурсивную версию (с именем rec-add) вычисления 
; суммы чисел заданного списка:
; без работы со структурированными смешанными списками
(defun rec-add-inner (lst acc)
    (cond 
        ((null (cdr lst)) (+ acc (car lst)))
        (t (rec-add-inner (cdr lst) (+ acc (car lst))))))

(defun rec-add (lst)
    (rec-add-inner lst 0))

; С использованием дополняемой рекурсии
(defun rec-add (lst)
    (cond
        ((null (cdr lst)) (car lst))
        (t (+ (car lst) (rec-add (cdr lst))))))

; с обработкой смешанных структурированных списков
(defun rec-add-inner (lst acc)
    (cond
        ((null lst) acc)
        ((listp (car lst)) (rec-add-inner (cdr lst) (rec-add-inner (car lst) acc )))
        ((numberp (car lst)) (rec-add-inner (cdr lst) (+ acc (car lst))))
        (t (rec-add-inner (cdr lst) acc))))

(defun rec-add (lst)
    (rec-add-inner lst 0))

; С использованием дополняемой рекурсии
(defun rec-add (lst)
    (cond
        ((null lst) 0)
        ((symbolp (car lst)) (rec-add (cdr lst)))
        ((listp (car lst)) (+ (rec-add (car lst)) (rec-add (cdr lst))))
        ((numberp (car lst)) (+ (car lst) (rec-add (cdr lst))))))


; 9  Написать рекурсивную версию с именем recnth функции nth
(defun rec-nth (index lst)
    (cond 
        ((or (< n 0) (null lst)) nil)
        ((zerop index) (car lst))
        (t (rec-nth (- index 1) (cdr lst)))))

; 10 Написать рекурсивную функцию allodd, которая возвращает t когда
; все элементы списка нечетные
; без работы с структурированными смешанными списками
(defun allodr-rec (lst cur-bool)
    (cond
        ((null cur-bool) nil)
        ((null lst))
        (t (allodr-rec (cdr lst) (oddp (car lst))))))

(defun allodr (lst)
    (cond ((null lst) Nil)
    (T (allodr-rec lst t))))

; для работы с структурированными смешанными списками
(defun allodr-rec (lst cur-bool)
    (cond
        ((null cur-bool) nil)
        ((null lst))
        ((listp (car lst)) (and (allodr-rec (car lst) t) (allodr-rec (cdr lst) cur-bool)))
        ((numberp (car lst)) (allodr-rec (cdr lst) (oddp (car lst))))
        (t (allodr-rec (cdr lst) cur-bool))))

(defun allodr (lst)
    (cond ((null lst) Nil)
    (T (allodr-rec lst t))))

; 11 Написать рекурсивную функцию, которая возвращает первое 
; нечетное число из списка (структурированного), возможно создавая 
; некоторые вспомогательные функции.

(defun is-odd(num)
    (cond ((eql num 0) Nil)
          ((eql num 1) t)
          ((>= num 2) (is-odd (- num 2)))))

(defun my-odd-rec (lst)
	(cond ((null lst) Nil)
          ((oddp (car lst)) (car lst))
	      (T (my-odd-rec (cdr lst))) ))


; +12 Используя cons-дополняемую рекурсию с одним тестом завершения, 
; написать функцию которая получает как аргумент список чисел, 
; а возвращает список квадратов этих чисел в том же порядке.

; одноуровневый список -- только числа
(defun get-sqr-list (lst)
    (cond
        ((null lst) nil)
        (t (cons (* (car lst) (car lst)) (get-sqr-list (cdr lst))))))

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

; Рекурсивно для смешанного структурированного списка &&&&&&&&&&&&
(defun get-sqr-list (lst)
    (cond 
        ((null lst) nil)
		((symbolp (car lst)) (cons (car lst) (get-sqr-list (cdr lst))))
        ((listp (car lst)) (cons (get-sqr-list (car lst)) (get-sqr-list (cdr lst))))
        ((numberp (car lst)) (cons (* (car lst) (car lst)) (get-sqr-list (cdr lst))))
        (t (get-sqr-list (cdr lst)))))
