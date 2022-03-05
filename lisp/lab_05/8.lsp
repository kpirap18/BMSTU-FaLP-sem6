;; Напишите функцию, которая умножает на заданное число-аргумент первый числовой
;; элемент списка из заданного 3-х элементного списка-аргумента, когда
;; a) все элементы списка --- числа,
;; 6) элементы списка -- любые объекты.

;; -- любок кол-во элементов списка
(defun f (lst num)
	(cond ((null lst) ())
	      ((numberp (car lst)) (cons (* num (car lst)) (cdr lst)))
          (T  (cons (car lst) (f (cdr lst) num)))
    ) 
)


;; -- 3 элемента
(defun f2 (lst num)
	(cond ((null lst) ())
	      ((numberp (car lst)) (cons (* num (car lst)) (cdr lst))) ;; умножили первый элемент
          ((numberp (cadr lst)) (cons (car lst) (cons (* num (cadr lst)) (cddr lst)))) ;; умножили второй элемент
          ((numberp (caddr lst)) (cons (car lst) (cons (cadr lst) (cons (* num (caddr lst)) (cdddr lst)) ))) ;; умножили третий элемент
    ) 
)


;;;;;;;;;;;;;;;;;;;
(defun f (lst num)
	(cond ((null lst) ())
          ((numberp (car lst))  (cons (* num (car lst)) (cdr lst))) 
          ((symbolp (car lst)) (cons (car lst) (f (cdr lst) num))) ;; если это символ 
          ((listp (car lst)) (cons (f (car lst) num)  (f (cdr lst) num) ))    
    ) 
)
 ;;; 
(defun f2 (num lst res)
	(cond	((null lst) res)
            ((numberp (car lst)) (cons (* num (car lst)) (cdr lst)))
			((listp (car lst)) (cons (f2 num (car lst) res) res))
			((symbolp (car lst)) (cons (car lst) (cons (f2 num (cdr lst) res) (cddr lst))))
	)
)

(defun mult-els-deep (lst num)
    (mapcar #'(lambda (arg) 
                      (cond 
                            ((listp arg) (mult-els-deep arg num))
                            ((numberp arg) (* arg num))
                            (t arg)
                      )
              ) lst
    )
)
