(setq M '( 
	(1 5 7)
	(2 4 8)
	(3 6 7)
	)
)

(defun difsum (M)
	(contar M '0 '0)
)

(defun contar (M nfila res)
	(if (null M) res
		(if (es-par nfila)
			(contar (cdr M) (+ 1 nfila) (sumar res (car M)))
			(contar (cdr M) (+ 1 nfila) (restar res (car M)))
		)
	)
)

(defun sumar (res L)
	(if (null L) 
		res
		(sumar (+ res (car L)) (cdr L))
	)
)


(defun restar (res L)
	(if (null L) 
		res
		(restar (- res (car L)) (cdr L))
	)
)

(defun es-par (num) 
	(eq 0 (mod num 2))
)