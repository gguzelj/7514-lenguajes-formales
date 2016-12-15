(setq M '( 
	(T NIL NIL NIL)
	(T T NIL NIL)
	(T T T NIL)
	(T T T T)
	)
)

(defun ejer (M)
	(ejer2 M '() '1)
)

(defun ejer2 (M resultado nro-fila)
	(if (null M)
		resultado
		(ejer2 (cdr M) (agregar-fila (car M) resultado nro-fila) (+ 1 nro-fila))
	)
)

(defun agregar-fila (fila resultado nro-fila)
	(if (> (contar-trues fila) (- (contar-trues fila) (length fila)))
		(cons nro-fila resultado)
		resultado
	)
)

(defun contar-trues (L)
	(if (null L) '0
		(contar-fila L '0)
	)
)

(defun contar-fila (F contador)
	(if (null F) contador
		(contar-fila (cdr F) (contar (car F) contador))
	)
)

(defun contar (F contador)
	(if (null F) contador
		(if F (+ 1 contador)
			contador
		)
	)
)