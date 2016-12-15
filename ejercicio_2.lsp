(defun ejerc2 (L i f)
	(reverse (sublist2 L '() i f '0))
)

(defun sublist2 (L resultado i f indice)
	(if (null L) resultado
		(if (and (>= indice i) (<= indice f))
			(sublist2 (cdr L) (cons (car L) resultado) i f (+ 1 indice))
			(sublist2 (cdr L) resultado i f (+ 1 indice))
		)
	)
)

(defun recorrer (L indice)
	(if (null L) indice
		(recorrer (cdr L) (+ 1 indice))
	)
)