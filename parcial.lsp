(setq M '( 
	(8 4 2 6)
	(A D G J)
	(7 1 3 9)
	(F R B I)
	)
)

(defun mostrar (M)
	(reverse(mostrarM-inv M nil))
)

(defun mostrarM (M res)
	(if (null (car M))
		res
		(mostrarM-inv (mapcar #'cdr M) (cons (mapcar #'car M) res))
	)
)

(defun mostrarM-inv (M res)
	(if (null (car M))
		res
		(mostrarM (mapcar #'cdr M) (cons (reverse(mapcar #'car M)) res))
	)
)