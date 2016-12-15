(setq M '( 
	(1 5 7 9)
	(2 4 8 4)
	(3 6 7 8)
	(6 8 7 3)
	)
)

(defun coparfilim (M)
	(tomar-filas-impares (tomar-columnas-pares M))
)

(defun tomar-columnas-pares (M)
	(transponer(reverse (tomar-columnas-pares2 M '() '1)))
)

(defun tomar-filas-impares (M)
	(reverse (tomar-filas-impares2 M '() '1))
)

(defun transponer (M)
	(reverse(transponer2 M nil))
)

(defun transponer2 (M res)
	(if (null (car M))
		res
		(transponer2 (mapcar #'cdr M) (cons (mapcar #'car M) res))
	)
)

(defun tomar-filas-impares2 (M res nfil)
	(if (null (car M)) res
		(if (es-par nfil)
			(tomar-filas-impares2 (cdr M) res (+ 1 nfil))
			(tomar-filas-impares2 (cdr M) (cons (car M) res) (+ 1 nfil))
		)
	)
)

(defun tomar-columnas-pares2 (M res ncol)
	(if (null (car M)) res
		(if (es-par ncol)
			(tomar-columnas-pares2 (elim-primer-col M) (cons (mapcar #'car M) res) (+ 1 ncol))
			(tomar-columnas-pares2 (elim-primer-col M) res (+ 1 ncol))
		)
	)
)

(defun elim-primer-col (M)
	(mapcar #'cdr M)
)

(defun es-par (nro)
	(eq 0 (mod nro 2))
)