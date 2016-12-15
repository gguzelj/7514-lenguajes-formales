(setq M '(
		(1 2 3 4)
		(5 6 7 8)
		(9 10 11 12)
		(13 14 15 16)
	)
)

(setq M2 '(
		(1 2 3 4 5)
		(A B C D E)
		(E F G H i)
		(6 7 8 9 10)
	)
)

(defun caracol (M)
	(reverse (caracol2 M nil))
)

(defun caracol2 (M resultado)
	(if (null (car M))
		resultado
		(caracol2 (transformar M) (append resultado (car M)))
	)
)

(defun transformar(M)
	(rotar(cdr M))
)

(defun rotar(M)
	(reverse (rotar2 M nil))
)

(defun rotar2(M res)
	(if (null (car M))
		res
		(rotar2 (mapcar #'butlast M) (cons (mapcar #'car (mapcar #'last M)) res))
	)
)

(defun test (M1 M2 M3)
	(mapcar #'func M1 M2)
)

(defun func (L1 L2)
	(mapcar #'func2 L1 L2)
)


(defun func (N1 N2)
	(+ N1 N2)
)