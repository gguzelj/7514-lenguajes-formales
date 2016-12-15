(setq M '( 
	(1 5 7 9)
	(2 4 8 4)
	(3 6 7 8)
	(6 8 7 3)
	)
)

(defun diagonalDer (M)
	(reverse(diagonalDer2 M '0 nil))
)

(defun diagonalDer2 (M index res)
	(if (null (car M))
		res
		(diagonalDer2 (cdr M) (+ 1 index) (cons (tomar-fila (car M) index) res))
	)
)

(defun tomar-fila (L from)
	(reverse(tomar-fila2 L from '0 nil))
)

(defun tomar-fila2 (L from curr res)
	(if (null L)
		res
		(if (>= curr from)
			(tomar-fila2 (cdr L) from (+ 1 curr) (cons (car L) res))
			(tomar-fila2 (cdr L) from (+ 1 curr) res)
		)
	)
)