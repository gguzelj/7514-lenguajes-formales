(defun intercal (L1 L2)
	(intercal2 (reverse L1) (reverse L2) nil)
)

(defun intercal2 (L1 L2 res)
	(if (or (null L1) (null L2))
		res
		(intercal2 (cdr L1) (cdr L2) (cons (car L1) (cons (car L2) res)))
	)
)