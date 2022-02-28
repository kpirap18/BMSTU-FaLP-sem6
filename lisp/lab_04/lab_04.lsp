; № 4
(defun play()
	(setq score1 (+ (random 6)))
	(setq score2 (+ (random 6)))
	(setq cur_sum (+ score1 score2))
	(cond ((or (= cur_sum 7) (= cur_sum 11)) 100)
		  ((or (= score1 score2 1) (= score1 score2 6))
				(and (print "another try") (play)))
		  (T cur_sum)
	)
)


(defun main ()
	(print "TURN: player 1")
	(setq res1 (play))
	(cond ((= res1 100) (print `(1 player WON!!!)))
		  (t (and (print "TURN: player 2")
				  (setq res2 (play))
				  (cond ((= res2 100) (print `(2 player WON!!!)))
						(t (cond ((> res1 res2) (print `(1 player WON!!!)))
								 ((= res1 res2) (print `TIE))
								 (t 			   (print `(2 player WON!!!)))
							)
						)
				  )
			  )
		   )
	)
	t
)




; 4
(defvar name-first)
(defvar name-second)

(setf name-first "One")
(setf name-second "Two")

;; dice = ((1-6 1-6) 1/0)
(defvar dice-first)
(defvar dice-second)
(defvar tmp-dice)

(defun roll-one-dice ()
	(+ (random 6) 1 ) )

(defun roll-two-dice ()
	(list (roll-one-dice) (roll-one-dice)) )

(defun sum (dice) 
	(+ (car dice) (cadr dice)) )

(defun is-win (dice) 
	(cond ((= (sum dice) 7 )) 
		((= (sum dice) 11)) ) )

(defun repeat-roll (dice)
	(cond ((= (car dice) (cadr dice) 6))
		((= (car dice) (cadr dice) 1))) )


(defun print-res (name dice) 
	(format Nil "~%Win ~a ~a ~a~%" name  (car dice) (sum (car dice))) )

(defun user-round (name)
	(setf tmp-dice (roll-two-dice))
	(format T "Player name: ~a ~a sum = ~a ~%" name tmp-dice (sum tmp-dice))
	(cond ((is-win tmp-dice) (list tmp-dice 1))
		((repeat-roll tmp-dice) (user-round name))
		(T (list tmp-dice 0))) )
		

(defun play ()
	(setf dice-first (user-round name-first))
	(if (= (cadr dice-first) 1) (print-res name-first dice-first)
	(and (setf dice-second (user-round name-second))
	(cond ((= (cadr dice-second) 1) (print-res name-second dice-second))
		((> (sum (car dice-first)) (sum (car dice-second))) (print-res name-first dice-first))
		((< (sum (car dice-first)) (sum (car dice-second))) (print-res name-second dice-second))
		((format Nil "Draw"))))))