

;(distl 'a '(b c d)) -> ((a b) (a c) (a d))
(defun distl (val L)
	(mapcar #'dist (copiar-val val (length L)) L)
)

(defun dist (val1 val2)
	(list val1 val2)
)

(defun copiar-val (val to)
	(copiar-val2 val to nil)
)

(defun copiar-val2 (val to res)
	(if (eq 0 to)
		res
		(copiar-val2 val (- to 1) (cons val res))
	)
)