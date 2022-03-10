; 1 Написать хвостовую рекурсивную функцию my-reverse, 
; которая развернет верхний уровень своего списка-аргумента lst
(defun move-to (lst res)
    (cond ((null lst) res)
          (T (move-to (cdr lst) (cons (car lst) res)))))

(defun my-reverse (lst)
    (move-to lst ()))

; 3 Написать функцию, которая возвращает первый 
;элемент списка -аргумента, который сам является непустым списком.

(defun ret-first-lst (lst)
    (cond ((null lst) lst)
          ((and (listp (car lst)) (not (null (car lst)))) (car lst))
          (t (ret-first-lst (cdr lst)))))

(defun ret-first-lst-fun (lst)
    (find-if #'(lambda (x) (and (listp x) (not (null x)))) lst))

; 4 Написать функцию, которая выбирает из заданного 
; списка только те числа, которые больше 1 и меньше 10. 
; (Вариант: между двумя заданными границами. )
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
(defun select-between (lst)
    (select-rec lst 1 10 ()))

; +7  Напишите рекурсивную функцию, которая умножает на заданное 
; число-аргумент все числа из заданного списка-аргумента, когда
; a) все элементы списка --- числа,
; 6) элементы списка -- любые объекты.


; +8  Напишите функцию, select-between, которая из списка-аргумента, 
; содержащего только числа, выбирает только те, которые расположены
; между двумя указанными границамиаргументами и возвращает их в виде
; списка (упорядоченного по возрастанию списка чисел (+ 2 балла)).


; +8 Написать рекурсивную версию (с именем rec-add) вычисления 
; суммы чисел заданного списка:
; а) одноуровнего смешанного,
; б) структурированного.


; +9  Написать рекурсивную версию с именем recnth функции nth


; +10 Написать рекурсивную функцию allodd, которая возвращает t когда
; все элементы списка нечетные


; +-11 Написать рекурсивную функцию, которая возвращает первое 
; нечетное число из списка (структурированного), возможно создавая 
; некоторые вспомогательные функции.

(defun my-odd-rec (lst)
	(cond ((null lst) num)
          ((oddp (car lst)) (car lst))
	      (T (my-odd-rec (cdr lst))) ))

(defun ret-last-odd-inner (lst num)
    (cond
        ((null lst) num)
        (t (ret-last-odd-inner (cdr lst) (cond 
                                            ((oddp (car lst)) (car lst))
                                            (t num))))))

(defun ret-last-odd (lst)
    (ret-last-odd-inner lst nil))

; +12 Используя cons-дополняемую рекурсию с одним тестом завершения, 
; написать функцию которая получает как аргумент список чисел, 
; а возвращает список квадратов этих чисел в том же порядке.
