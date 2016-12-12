;Ejemplos:

; con numeros
;(evaluar '2 nil) => 2
;
; con valores booleanos true false
;(evaluar nil nil) => nil
;(evaluar 't nil) => t
;
;asociaciones en el ambiente
;(evaluar 'A '(A 2) ) => 2
;(evaluar 'B '(A 2 B 10) ) => 10
;
;la función quote
;(evaluar '(quote A) nil) => A
;(evaluar '(quote 1) nil) => 1
;(evaluar '(quote (car a)) nil ) => (car a)
;(evaluar '(quote ((2 3) (4 5))) nil) => ((2 3) (4 5))
;
;funciones booleanas and y or
;(evaluar '(and (or t nil) t) nil ) => t
;(evaluar '(and (or t nil) (or nil nil)) nil) => nil
;(evaluar '(or (or t nil) (or nil nil )) nil) t
;
;Función car + ambiente
;(evaluar '(car (list a 2 3)) '(a 100) ) => 100
;
;Función cdr + ambiente
;(evaluar '(cdr (list a b c)) '(a 100 b 99 c 98) ) => (99 98)
;
;Funciones anónimas lambda
;(evaluar '((lambda (x) (* x 2)) 2) nil ) => 4
;(evaluar '((lambda (x y) (+ (* x 2) y)) 2 4) nil) => 8
;(evaluar '(lambda (x) (* x 2)) nil) => (lambda (x) (* x 2))
;(evaluar '(sumar '2) '(sumar (lambda (x) (+ x 1)))) => 3
;
;Forma funcional mapcar / reduce
;(evaluar '(mapcar 'numberp numeros) '(numeros (4 5 6 nil))) => (t t t nil)
;(evaluar '(mapcar 'car (quote ( (2 3) (4 5 ))) ) nil) => (2 4)
;(evaluar '(reduce '+ numeros) '(numeros (0 1 2 3 4 5 6 7 8 9))) => 45
;(evaluar '(mapcar sig '(1 2 3)) '(sig (lambda(x)(+ x 1)))) => (2 3 4)
;(evaluar '(mapcar 'list '(1 2 3)'(4 5 6)) nil) => ((1 4) (2 5) (3 6))
;(evaluar '(reduce '+ numeros) '(numeros (0 1 2 3 4 5 6 7 8 9)))
;
;Otros ejemplos:
;(evaluar '(quote FG) nil) => FG
;(evaluar '(list a 2 b 4) '(a 1 b 3)) => (1 2 3 4)
;(evaluar '(if (eq a (+ b 1)) (or t (and t t)) (and nil t)) '(a 2 b 1)) => T
;(evaluar '(if (eq a (+ b 1)) (or t (and t t)) (and nil t)) '(a 2 b 2)) => NIL
;(evaluar '(suma x y) '(suma (lambda (a b) (+ a b)) x 5 y 3)) => 8
;(evaluar '(cdr (quote (x y z))) nil) => (y z)
;(evaluar '(if (eq x (+ 2 y)) (suma 2 x) (suma z y)) '(suma (lambda (a b) (+ a b)) x 5 y 3 z 9))) => 7
;(evaluar '(mapcar 'car l) '(l ((a b c) (1 2 3) (x y z)) )) => (A 1 X)
;(evaluar '(sumar_1 15) '(sumar_1 (lambda (x) (+ x 1)))) => 16
;(evaluar '(if (eq a b) (suma a b) (resta a b)) '(a 2 b 2 suma (lambda (x y) (+ x y)) resta (lambda (x y) (- x y)))) => 4
;(evaluar '(if (eq a b) (suma a b) (resta a b)) '(a 2 b 1 suma (lambda (x y) (+ x y)) resta (lambda (x y) (- x y)))) => 1
;(defun fact (X) (cond	(( < X 2) 1) (t (* X (fact (- X 1))))))
;(evaluar '(mapcar 'fact numeros) '(numeros (0 1 2 3 4 5 6 ) )) ) => (1 1 2 6 24 120 720)
;(evaluar '(mapcar sig '(1 2 3)) '(sig (lambda(x)(+ x 1))))
;(evaluar '(mapcar 'list '(1 2 3)'(4 5 6)) nil)
;(evaluar '(mapcar 'fact numeros) '(numeros (0 1 2 3 4 5 6 ) fact (lambda (X)(cond (( < X 2) 1) (t (* X (fact (- X 1))))))))

;(evaluar '(reduce 'suma numeros) '(numeros (0 1 2 3 4 5 6 7 8 9) suma (lambda (x y) (+ x y))))
(defun evaluar (exp amb) 
	(if (atom exp) (evaluar-atom exp amb)
		(cond 
			((eq (car exp) 'quote)	(evaluar-quote exp amb))
			((eq (car exp) 'and)	(evaluar-and exp amb))
			((eq (car exp) 'or) 	(evaluar-or exp amb))
			((eq (car exp) 'if) 	(evaluar-if exp amb))
			((eq (car exp) 'lambda) (evaluar-lambda exp amb))
			((eq (car exp) 'cond)	(evaluar-cond exp amb))
			((eq (car exp) 'while)	(evaluar-while exp amb))
			((eq (car exp) 'mapcar)	(evaluar-mapcar (evaluar (cadr exp) amb) (evaluar-arg (cddr exp) amb) amb))
			((eq (car exp) 'reduce)	(evaluar-reduce (evaluar (cadr exp) amb) (evaluar-arg (cddr exp) amb) amb))
			(t (aplicar (car exp)	(evaluar-arg (cdr exp) amb) amb))
		)
	)
)

;funciona con todo.. monadico, nadico.. todo
(defun evaluar-mapcar (fn listas amb)
	(if (null (car listas)) nil
		(cons (aplicar fn (mapcar 'car listas) amb) (evaluar-mapcar fn (mapcar 'cdr listas) amb))
	)
)

(defun evaluar-reduce (fn listas amb)
	(if (null (cddar listas)) (aplicar-reduce fn listas amb)
		(evaluar-reduce fn (list (cons (aplicar-reduce fn listas amb) (cddar listas))) amb)
	)
)

(defun aplicar-reduce (fn listas amb)
	(aplicar fn (list (caar listas) (cadar listas)) amb)
)

(defun aplicar (fn lae amb)
	(if (atom fn) 
		(cond 
			((eq fn 'car)		(caar lae))
			((eq fn 'cadr)		(cadar lae))
			((eq fn 'cdr)		(cdar lae))
			((eq fn 'cons)		(cons (car lae) (cadr lae)))
			((eq fn 'list)		lae) ;lae ya es una lista de los argumentos. No hay que hacer nada
			((eq fn 'append)	(append (car lae) (cadr lae)))
			((eq fn '+)			(+ (car lae) (cadr lae)))
			((eq fn '-)			(- (car lae) (cadr lae)))
			((eq fn '*)			(* (car lae) (cadr lae)))
			((eq fn '/)			(/ (car lae) (cadr lae)))
			((eq fn '<)			(< (car lae) (cadr lae)))
			((eq fn '>)			(> (car lae) (cadr lae)))
			((eq fn '>=)		(>= (car lae) (cadr lae)))
			((eq fn '<=)		(<= (car lae) (cadr lae)))
			((eq fn '=)			(= (car lae) (cadr lae)))
			((eq fn 'eq)		(eq (car lae) (cadr lae)))
			((eq fn 'not)		(not (car lae)))
			((eq fn 'apply) 	(apply (car lae) (cadr lae)))
			((eq fn 'atom)		(atom (car lae)))
			((eq fn 'listp)		(listp (car lae)))
			((eq fn 'symbolp)	(symbolp (car lae)))
			((eq fn 'numberp)	(numberp (car lae)))
			((eq fn 'null)		(null (car lae)))

			(T  (aplicar (buscar fn amb) lae amb))
		)
		(evaluar (caddr fn) (aplicar-asoc (cadr fn) lae amb))
	)
)

(defun aplicar-asoc (fn lae amb)
	(if (null fn) amb
		(append (list (car fn) (car lae)) (aplicar-asoc (cdr fn) (cdr lae) amb))
	)
)


(defun evaluar-arg (exp amb)
	(if (null exp) nil
		(cons (evaluar (car exp) amb) (evaluar-arg (cdr exp) amb))
	)
)

;Ejemplos:
;(evaluar '(cond (nil 1) (nil 2) (t 3)) nil)
;(evaluar '(cond (nil 1) ((and t t) 2) (t 3)) nil)
;(evaluar '(cond (nil 1) ((and t t) (or t nil)) (t 3)) nil)
;(evaluar '(cond (nil 1) ((and t t) (or nil nil)) (t 3)) nil)
(defun evaluar-cond (exp amb)
	(evaluar-cond2 (cdr exp) amb )	
)

(defun evaluar-cond2 (condiciones amb)
	(if (evaluar (caar condiciones) amb)
		(evaluar (cadar condiciones) amb)
		(evaluar-cond2 (cdr condiciones) amb)
	)	
)

;'(while (lambda(x)(NoCero(car x))) (lambda (x) (list (Restart1 (car x) (* (car x) (cadr x)))) ) (car '((5 1) 8 7)) ) 
;(evaluar '(while (lambda(x)(NoCero(car x))) (lambda (x) (list (Restar1 (car x)) (* (car x) (cadr x))) ) (car '((5 1) 8 7)) ) '(NoCero (lambda(x) (not (eq x 0))) Restar1 (lambda(n) (- n 1))))
(defun evaluar-while (exp amb)
	(if (evaluar (list (cadr exp) (cadddr exp)) amb)
		(evaluar (list (car exp) (cadr exp) (caddr exp) (evaluar (list (caddr exp) (cadddr exp)) amb)) amb)
		(cadddr exp)
	)
)

(defun evaluar-lambda (exp amb)
	exp
)

;Ejemplos:
;(evaluar '(if t 2 1) nil)
;(evaluar '(if (and t t) 2 1) nil)
;(evaluar '(if (and nil t) 2 1) nil)
;(evaluar '(if (or nil t) 2 1) nil)
(defun evaluar-if (exp amb)
	(if (evaluar (cadr exp) amb)
		(evaluar (caddr exp) amb)
		(evaluar (cadddr exp) amb)
	)	
)

(defun evaluar-quote (exp amb)
	(cadr exp)
)

;Ejemplos:
;(evaluar '(and (and t t) t) nil )
;(evaluar '(and t nil) nil )
;(evaluar '(or (or t nil) (or nil nil )) nil)
(defun evaluar-and (exp amb)
	(if (evaluar (cadr exp) amb) 
		(evaluar (caddr exp) amb) 
		nil
	)
)

(defun evaluar-or (exp amb)
	(if (evaluar (cadr exp) amb)
		t 
		(evaluar (caddr exp) amb) 
	)
)

(defun evaluar-atom (exp amb)
	(cond
		((eq exp 't) t)
		((numberp exp) exp)
		(t (buscar exp amb))
   )
)


;Funcion que busca una expresion en el ambiente.
;El ambiente esta definido la siguiente manera: 
; '(label1 value1 label2 value2)
(defun buscar (exp amb)
	(if (null amb)
		nil
		(if (eq exp (car amb))
			(cadr amb)
			(buscar exp (cddr amb))
		)
	)
)

 
;(trace evaluar-while)
;(trace evaluar)
;(trace evaluar-arg)
;(trace aplicar)
;(trace aplicar-asoc)
;(trace evaluar-atom)
;(trace evaluar-quote)
;(trace evaluar-and)
;(trace evaluar-or)
;(trace evaluar-if)
;(trace evaluar-lambda)
;(trace evaluar-mapcar)
;(trace evaluar-reduce)
;(trace evaluar-cond)
;(trace evaluar-while)
;(trace evaluar-arg)