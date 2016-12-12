(defun run (prg ent &optional (mem nil))
    (if (null prg) nil
        (cond 
            ((eq (caar prg) 'int) (run (cdr prg) ent (agregar-var (cdar prg) mem)))
            ((eq (caar prg) 'main) (ejec (cadar prg) ent mem))
            (t 'ERROR_FALTA_MAIN)
        )
    )
)

;Analizamos la lista de variables a declarar, y las agregamos a memoria.
;Ejemplos:
;(agregar-var '(x) nil)
;((X 0))
;(agregar-var '(z A = 10) nil)
;((A 10) (Z 0))
;(agregar-var '(a = 2 b = 3 c d = 1) nil)
;((D 1) (C 0) (B 3) (A 2))
(defun agregar-var (vars mem)
    (if (null vars) mem
        (if (eq (cadr vars) '=)
            (agregar-var (cdddr vars) (cons (list (car vars) (caddr vars)) mem))
            (agregar-var (cdr vars) (cons (list (car vars) 0) mem))
        )
    )
)

;Funcion para asignar algun valor a la memoria
;(asignar-a-mem 'd '2 '((D 1) (C 0) (B 3) (A 3)))
;((D 2) (C 0) (B 3) (A 3))
(defun asignar-a-mem (var val mem &optional (resultado nil))
    (if (null mem) (reverse resultado)
        (if (eq var (caar mem))
            (asignar-a-mem var val (cdr mem) (cons (list var val) resultado))
            (asignar-a-mem var val (cdr mem) (cons (car mem) resultado))
        )
    )
)

(defun ejec (prg ent mem &optional (sal nil))
    (if (null prg) sal
        (cond
            ; PRINTF
            ((eq (caar prg) 'printf) (handle-printf prg ent mem sal))

            ; SCANF
            ((eq (caar prg) 'scanf) (handle-scanf prg ent mem sal))
            
            ; IF
            ((eq (caar prg) 'if) (handle-if prg ent mem sal))

            ; WHILE
            ((eq (caar prg) 'while) (handle-while prg ent mem sal))
            
            ;asignacion
            ((esasignacion (car prg) mem) (handle-asignacion prg ent mem sal))

            (t (list 'ERROR  prg))  
        )
    )
)

(defun handle-if (prg ent mem sal)
    (if (tiene-asignaciones (cadar prg))
        (ejec (guardar-asignaciones (cdr prg) (cadar prg)) ent mem sal)
        (if (valor (cadar prg) mem)
            (ejec (append (caddar prg) (cdr prg)) ent mem sal)
            (if (eq 5 (length (car prg))) ;Tiene else
                (ejec (append (cadr (cdddar prg)) (cdr prg)) ent mem sal)
                (ejec (cdr prg) ent mem sal)
            )
        )
    )
)

(defun guardar-asignaciones (prg exp &optional (asignaciones nil))
    (if (null exp)
        (append (reverse asignaciones) prg)
        (if (esasignacion2 (cdr exp))
            (guardar-asignaciones prg (cdr exp) (cons exp asignaciones))
            (if (listp (cadr exp))
                (guardar-asignaciones prg (cadr exp) asignaciones)
                (guardar-asignaciones prg (cdr exp) asignaciones)
            )
        )
    )
)

(defun tiene-asignaciones (exp)
    (if (null exp)
        nil
        (if (esasignacion2 (cdr exp))
            T
            (if (listp (cadr exp))
                (tiene-asignaciones (cadr exp))
                (tiene-asignaciones (cdr exp))
            )
        )
    )
)

(defun handle-while (prg ent mem sal)
    (if (valor (cadar prg) mem)
        (ejec (append (caddar prg) prg) ent mem sal)
        (ejec (cdr prg) ent mem sal)
    )
)

(defun handle-printf (prg ent mem sal)
    (if (stringp (cadar prg))
        (ejec (cdr prg) ent mem (append sal (list (cadar prg))))
        (ejec (cdr prg) ent mem (append sal (list (valor (cdar prg) mem))))
    )
)

(defun handle-scanf (prg ent mem sal)
    (ejec (cdr prg) (cdr ent) (asignar-a-mem (cadar prg) (car ent) mem) sal)
)

(defun handle-asignacion (prg ent mem sal)
    (cond
        ((eq (cadar prg) '=) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (cddar prg) mem) mem) sal))
        ((eq (cadar prg) '+=) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (append (list (caar prg) '+)(list(cddar prg))) mem) mem) sal))
        ((eq (cadar prg) '-=) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (append (list (caar prg) '-)(list(cddar prg))) mem) mem) sal))
        ((eq (cadar prg) '*=) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (append (list (caar prg) '*)(list(cddar prg))) mem) mem) sal))
        ((eq (cadar prg) '/=) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (append (list (caar prg) '/)(list(cddar prg))) mem) mem) sal))
        ((eq (cadar prg) '++) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (list (caar prg) '+ '1) mem) mem) sal))
        ((eq (cadar prg) '--) (ejec (cdr prg) ent (asignar-a-mem (caar prg) (valor (list (caar prg) '- '1) mem) mem) sal))
        ((eq (caar prg) '++) (ejec (cdr prg) ent (asignar-a-mem (cadar prg) (valor (list (cadar prg) '+ '1) mem) mem) sal))
        ((eq (caar prg) '--) (ejec (cdr prg) ent (asignar-a-mem (cadar prg) (valor (list (cadar prg) '- '1) mem) mem) sal))
        (t 2)
    )
)

;Determinamos si una sentencia es de asignacion. Existen distintos tipos de asignaciones:
;(esasignacion '(a = 1) '((a 0)))
;T
;(esasignacion '(a = (a + 1) * 4) '((a 0)))
;T
;(esasignacion '(a -= 5) '((a 0)))
;T
;(esasignacion '(a *= 3) '((a 0)))
;T
;(esasignacion '(a++) '((a 0)))
;T
;(esasignacion '(--a) '((a 0)))
;T
(defun esasignacion (exp memoria)
    (if (eq (buscar_variable (car exp) memoria) 'ERROR_VARIABLE_NO_DECLARADA)
        (if (or (eq (car exp) '++) (eq (car exp) '--))
            (not (eq (buscar_variable (cadr exp) memoria) 'ERROR_VARIABLE_NO_DECLARADA))
            nil
        )
        (esasignacion2 (cdr exp))
    )
)

(defun esasignacion2 (exp)
    (cond
        ((eq (car exp) '=) t)
        ((eq (car exp) '+=) t)
        ((eq (car exp) '-=) t)
        ((eq (car exp) '*=) t)
        ((eq (car exp) '/=) t)
        ((eq (car exp) '++) t)
        ((eq (car exp) '--) t)
        (t nil)
    )
)

(defun operar (operador val1 val2)
    (cond
        ((eq operador '==) (eq val1 val2))
        ((eq operador '!=) (not (eq val1 val2)))
        ((eq operador '&&) (and (not (eq 0 val1)) (not (eq 0 val2))))
        ((eq operador '||) (or  (not (eq 0 val1)) (not (eq 0 val2))))
        ((es_operador operador) (apply operador (list val1 val2)))
        (t nil)
    )
)

(defun valor (exp mem &optional(operadores nil) (operandos nil))
    (if (and (atom exp) (not (null exp))) (if (numberp exp) exp (buscar_variable exp mem))
        (if (null exp)
            (if (null operadores)
                (car operandos)
                (valor exp mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
            )
            (if (es_operador (car exp))
                (if (null operadores)
                    (valor (cdr exp) mem (cons (car exp) operadores) operandos)
                    (if (< (peso (car operadores)) (peso (car exp)))
                        (valor (cdr exp) mem (cons (car exp) operadores) operandos)
                        (valor exp mem (cdr operadores) (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos)))
                    )
                )
                (valor (cdr exp) mem operadores (cons (valor (car exp) mem) operandos))
            )
        )
    )
)

(defun peso (x)
    (cond
        ((eq x '*) 10)
        ((eq x '/) 10)
        ((eq x '+) 5)
        ((eq x '-) 5)
        ((eq x '>) 5)
        ((eq x '>) 5)
        ((eq x '>=)5)
        ((eq x '<=) 5)
        ((eq x '==) 1)
        ((eq x '!=) 1)
        (t 0)
    )
)

(defun es_operador (operador)
    (if (null operador) nil
        (cond 
            ((equal operador '+) t)
            ((equal operador '-) t)
            ((equal operador '*) t)
            ((equal operador '/) t)
            ((equal operador '>=) t)
            ((equal operador '>) t)
            ((equal operador '<=) t)
            ((equal operador '<) t)
            ((equal operador '==) t)
            ((equal operador '!=) t)
            (t nil)
        )
    )
)

;Buscamos una variable dentro de la memoria
;(buscar_variable 'B '((D 1) (C 0) (B 3) (A 2)) )
;3
(defun buscar_variable (var mem)
    (if (null mem)
        'ERROR_VARIABLE_NO_DECLARADA
        (if (eq var (caar mem))
            (cadar mem)
            (buscar_variable var (cdr mem))
        )
    )
)


(setq main1 '( 
    (int z = 2)
    (int b = 2)
    (int c = 2)
    )
)

(setq main2 '( 
        (int a = 2 b = 3)
        (main (
            (printf (a + b))
            (printf b)
            )
        )
    )
)

;(run main3 nil)
;(ERROR_VARIABLE_NO_DECLARADA)
(setq main3 '( 
    (int z = 2)
    (main (
        (printf "El numero Z es")
        (printf z)
        )
    )
))

;(run main4 '(5))
;(5)
(setq main4 '( 
    (int z = 2)
    (main (
        (scanf b)
        (printf b)
        )
    )
))

;(run main5 '(5))
;(8)
(setq main5 '( 
    (int a = 2 b)
    (main (
        (scanf b)
        (a = b + 3)
        (printf a)
        )
    )
))

;(run main5_2 '(6))
;(8)
(setq main5_2 '(
    (int a = 2 b)
    (main (
        (a = (a + 1) * 4)
        (b -= 5)
        (a += 3)
        (printf a)
        (scanf a)
        (printf a)
        (printf b)
        )
    )
))


;(run main6 nil)
;(3)
(setq main6 '( 
    (int a = 2 b)
    (main (
        (a = 3)
        (printf a)
        )
    )
))

;(run main7 nil)
;(2)
(setq main7 '( 
    (int a = 2 b)
    (main (
        (-- a)
        (++ a)
        (a --)
        (a ++)
        (a += 1)
        (a -= 1)
        (a *= 1)
        (a /= 1)
        (printf a)
        )
    )
))

;(run main8 nil)
;(2)
(setq main8 '( 
    (int a = 2)
    (main (
        (if (a == 2)
            ( (printf a) 
            )
        )
        )
    )
))

;(run main9 nil)
;(2)
(setq main9 '( 
    (int a = 3)
    (main (
        (if (a == 2)
            ( (printf a) 
            )
        )
        )
    )
))

;(run main10 nil)
;(5 6 4 5)
(setq main10 '( 
    (int a = 5)
    (main (
        (printf a) 
        (++ a)
        (printf a) 
        (if (a == 2)
            ( 
                (++ a)
                (printf a) 
            )
        else
            (
                (a --)
                (a --)
                (printf a)
            )
        )
        (++ a)
        (printf a) 
        )
    )
))

;(run main11 '(700 100))
;(121 893 700 193 100)
(setq main11 '( 
    (int x y p = 10)
    (int r)
    (main ( 
        (x = p + 10)
        (p ++)
        (++ x)
        (x *= p - 4)
        (if (x < p)
            ( 
                (printf (x + p))
                (scanf y)
            )
        else 
            ( 
                (x = x * 6)
                (printf (p * p))
            )
        )
        (while (x > p * 10)
            (
                (printf (x + p))
                (scanf y)
                (printf y)
                (x -= y)
            )
        )
        )
    )
))

;(run main12 '(5))
;(120)
(setq main12 '( 
    (int n fact = 1)
    (main (
        (scanf n)
        (if (n < 0 )
            ( 
                (printf "no existe fact de nro negativo" )
            )
        else 
            (
                (while (n > 1)
                    (
                        (fact = fact * n) (n -- )
                    )
                ); cierra while
                (printf fact )
            ); cierra else
        ); cierra if
        )
    ); cierra main
))

;(run main nil)
;(2)
(setq main20 '( 
    (int a b c)
    (main (
        (a = 2)
        (b = 3)
        (c = 4)
        (if (c > 11 * (a = 5 * (b = b + 1 + (c = 10))))
            ( 
                (printf 111)  
            )
        )
        (printf a) 
        (printf b) 
        (printf c) 
        )
    )
))

;(trace run)
;(trace ejec)
;(trace valor)
;(trace handle-if)
;(trace handle-while)
;(trace handle-asignacion)
;(trace asignar-a-mem)
;(trace operar)

;(trace tiene-asignaciones)
;(trace guardar-asignaciones)
