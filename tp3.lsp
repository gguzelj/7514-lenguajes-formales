(defun run (prg ent &optional (mem nil))
    (if (null prg) nil
        (cond 
            ((eq (caar prg) 'int) (run (cdr prg) ent (agregar-var (cdar prg) mem)))
            ((eq (caar prg) 'main) (ejec (cadar prg) ent mem))
            ;Las funciones solo pueden ser del tipo void (por ahora...)
            ((eq (caar prg) 'void) (run (cdr prg) ent (agregar-fun (car prg) mem)))
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

(defun agregar-fun (fun mem)
    (if (null fun) mem
        (cons (list (cadr fun) fun) mem)
    )
)

;Funcion para asignar algun valor a la memoria
;(asignar-a-mem 'd '2 '((D 1) (C 0) (B 3) (A 3)))
;((D 2) (C 0) (B 3) (A 3))
(defun asignar-a-mem (var val mem &optional (aux nil))
    (if (null mem) aux
        (if (eq var (caar mem))
            (append (reverse aux) (cons (list var val) (cdr mem)))
            (asignar-a-mem var val (cdr mem) (cons (car mem) aux))
        )
    )
)

;    (if (null mem) (reverse resultado)
;        (if (eq var (caar mem))
;            (asignar-a-mem var val (cdr mem) (cons (list var val) resultado))
;            (asignar-a-mem var val (cdr mem) (cons (car mem) resultado))
;        )
;    )

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

            ((eq (caar prg) 'int) (ejec (cdr prg) ent (agregar-var (cdar prg) mem) sal))

            ((eq (caar prg) 'FIN_FUN) (ejec (cdr prg) ent (limpiar-mem mem) sal))
            
            ;asignacion
            ((esasignacion (car prg) mem) (handle-asignacion prg ent mem sal))

            (t (handle-funcall prg ent mem sal))
            ;(t (list 'ERROR  prg))  
        )
    )
)

;Removemos del stack todas las variables asignadas por una funcion
(defun limpiar-mem (mem)
    (if (null mem) nil
        (if (eq 'FIN_FUN (caar mem))
            (cdr mem)
            (limpiar-mem (cdr mem))
        )
    )
)

(defun handle-funcall (prg ent mem sal)
    (if (eq (buscar_variable (caar prg) mem) 'ERROR_VARIABLE_NO_DECLARADA)
        (list 'ERROR_FUN_NOT_FOUND  prg)
        (handle-funcall2 prg ent mem sal (buscar_variable (caar prg) mem))
    )
)

(defun handle-funcall2 (prg ent mem sal fun)
    (ejec (append (cadddr fun) (cons (list 'FIN_FUN) (cdr prg))) ent (build-funcall-arg mem (caddr fun) (cdar prg)) sal)
)

;Marcamos en el stack la posicion donde finaliza la funcion.
;Luego agregamos los argumentos evaluados
(defun build-funcall-arg (mem args valores)
    (build-funcall-arg2 (cons (list 'FIN_FUN 'FIN_FUN) mem) args valores)
)

;Agregamos a memoria los argumentos evaluados
;mem es una lista con las variables definidas
;args son los argumentos que recibe la funcion, en la forma:
; '(int x int y)
;valores son las expresiones a evaluar para asignar a los argumentos 
(defun build-funcall-arg2 (mem args valores)
    (if (null valores) mem
        (build-funcall-arg2 (cons (list (cadr args) (valor (car valores) mem)) mem) (cddr args) (cdr valores))
    )
)

(defun handle-if (prg ent mem sal)
    (if (valor (cadar prg) mem)
        (ejec (append (caddar prg) (cdr prg)) ent mem sal)
        (if (eq 5 (length (car prg))) ;Tiene else
            (ejec (append (cadr (cdddar prg)) (cdr prg)) ent mem sal)
            (ejec (cdr prg) ent mem sal)
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
    (if (eq (buscar_variable (caar prg) mem) 'ERROR_VARIABLE_NO_DECLARADA)
        (list 'ERROR_FUN_NOT_FOUND  prg)
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

(defun operar (operador val1 val2 &optional(mem nil))
    (cond
        ((eq operador '=) (asignar-a-mem val1 val2 mem))
        ((eq operador '==) (eq val1 val2))
        ((eq operador '!=) (not (eq val1 val2)))
        ((eq operador '&&) (and (not (eq 0 val1)) (not (eq 0 val2))))
        ((eq operador '||) (or  (not (eq 0 val1)) (not (eq 0 val2))))
        ((es_operador operador) (apply operador (list val1 val2)))
        (t nil)
    )
)


(defun peso (x)
    (cond
        ((eq x '*) 10)
        ((eq x '/) 10)
        ((eq x '+) 6)
        ((eq x '-) 6)
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
    (int a b)
    (main (
        (a = 1)
        (if (2 < (a = 3 * 2))
            ( 
                (printf 111)  
            )
        )
        (printf a) 
        )
    )
))

(setq main2 '( 
    (int a b)
    (main (
        (a = 1)
        (b = 1)
        (if (2 < (a = 3 * (b = a + b)))
            ( 
                (printf 111)(a ++) (b ++) 
                (printf a)(printf b) 
            )
        )
        (printf a) 
        (printf b) 
        )
    )
))

(setq main3 '( 
    (int a b c)
    (main (
        (a = 1) 
        (c = 3)
        (b = 1)
        (if ((a = 2 *(b = 5)) <  (a = 3 * (b = a + b))+ (c = b))
            ( 
                (printf 111)
                (printf a)(printf b) (printf c)
            )
        )
        (printf a) 
        (printf b)(printf c) 
        )
    )
))



(defun es_operador2 (operador)
    (if (null operador) nil
        (if (eq operador '=)
            t
            (es_operador operador)
        )
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

;Funcion S que recibe una expresion, un estado de la memoria y devuelve
;como respuesta el resultado de ejecutar esa expresion y el estado de la memoria
(defun s (exp mem &optional(opr nil) (opn nil))
    (if (and (atom exp) (not (null exp))) (if (numberp exp) (list exp mem) (if (eq exp t) (list t mem) (list (buscar_variable exp mem) mem)))
        (if (null exp)
            (if (null opr)
                (if (null (car opn))
                    (list nil mem)
                    (s (car opn) mem)
                )
                (s_operar exp mem opr opn)
            )
            (if (es_operador (car exp))
                (if (null opr)
                    (s (cdr exp) mem (cons (car exp) opr) opn)
                    (if (< (peso (car opr)) (peso (car exp)))
                        (s (cdr exp) mem (cons (car exp) opr) opn)
                        (s_operar exp mem opr opn)
                    )
                )
                (if (esasignacion2 (list (cadr exp)))
                    (asig2 (car exp) (cadr exp) (s (cddr exp) mem) opr opn)
                    (s2 (cdr exp) opr opn (s (car exp) mem))
                )
            )
        )
    )
)

(defun asig2 (var op resultado opr opn)
    (cond
        ((eq op '=)  (s var (asignar-a-mem var (car resultado) (cadr resultado)) opr opn))
        ((eq op '+=) (s var (asignar-a-mem var (apply '+ (list (buscar_variable var (cadr resultado)) (car resultado))) (cadr resultado)) opr opn))
        ((eq op '-=) (s var (asignar-a-mem var (apply '- (list (buscar_variable var (cadr resultado)) (car resultado))) (cadr resultado)) opr opn))
        ((eq op '*=) (s var (asignar-a-mem var (apply '* (list (buscar_variable var (cadr resultado)) (car resultado))) (cadr resultado)) opr opn))
        ((eq op '/=) (s var (asignar-a-mem var (apply '/ (list (buscar_variable var (cadr resultado)) (car resultado))) (cadr resultado)) opr opn))
        (t nil)
    )
)

(defun s_operar (exp mem opr opn) 
    (s_operar2 exp mem opr opn (s (car opn) mem))
)

(defun s_operar2 (exp mem opr opn resultado)
    (s_operar3 exp (cadr resultado) opr opn (car resultado) (s (cadr opn) (cadr resultado))) 
)

(defun s_operar3 (exp mem opr opn op2 resultado) 
    (s exp (cadr resultado) (cdr opr) (cons (operar (car opr) (car resultado) op2) (cddr opn)))
)


(defun asig (var resultado opr opn)
    (s var (asignar-a-mem var (car resultado) (cadr resultado)) opr opn)
)

(defun s2 (exp opr opn resultado)
    (s exp (cadr resultado) opr (cons (car resultado) opn))
)

;(valor '(a * c + ( a + b ) * ( a - b ) + ( a * ( a * a * ( b - c * ( b * c - 23) - c ) + c ) - c ) + 45) '((a 23) (b 12) (c 3)))
;-364445


;(trace asig2)
;(trace run)
;(trace agregar-asignaciones)
;(trace s)
;(trace s2)
;(trace asig)
;(trace s_operar)
;(trace s_operar2)
;(trace s_operar3)
;(trace valor)
;(trace mapToValues)
;(trace es_operador2)
;(trace agregar-fun)
;(trace handle-if)
;(trace handle-while)
;(trace esasignacion)
;(trace handle-asignacion)
;(trace asignar-a-mem)
;(trace operar)
;(trace buscar_variable)
;(trace handle-funcall)
;(trace handle-funcall2)
;(trace ejec)
;(trace build-funcall-arg)