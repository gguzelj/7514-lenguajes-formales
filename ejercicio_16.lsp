(setq M '( 
	(1 5 7 9)
	(2 4 8 4)
	(3 6 7 8)
	(6 8 7 3)
	)
)

(defun diagonal (M)
	(reverse (diagonal2 M '0 (length M) nil))
)

(defun diagonal2 (M from to res)
	(if (eq from to)
		res
		(diagonal2 (cdr M) (+ 1 from) to (cons (tomar from (car M)) res) )
	)
)

(defun tomar (index L)
	(tomar2 index L '0)
)

(defun tomar2 (index L curr)
	(if (eq index curr)
		(car L)
		(tomar2 index (cdr L) (+ 1 curr))
	)
)