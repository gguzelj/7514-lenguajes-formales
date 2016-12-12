(setq grafo '(
	(a (b f)) 
	(b (a l )) 
	(c (b d )) 
	(d (c n e)) 
	(e (d)) 
	(f (g ))
	(g (h)) 
	(h (i l)) 
	(i (m j)) 
	(j (k)) 
	(k (o))
	(l (b f)) 
	(m (l c)) 
	(n (j m)) 
	(o (e n)))
)

(setq diccionario '(
	(a (PaseoColon Independencia))
	(b (PaseoColon Chile))
	(c (PaseoColon Mexico))
	(d (PaseoColon Venezuela))
	(e (PaseoColon Belgrano))
	(f (Independencia Balcarce))
	(g (Independencia Defensa))
	(h (Defensa Chile))
	(i (Defensa Mexico))
	(j (Defensa Venezuela))
	(k (Defensa Belgrano))
	(l (Balcarce Chile ))
	(m (Balcarce Mexico))
	(n (Balcarce Venezuela))
	(o (Balcarce Belgrano))
	) 
)

;(GPS '(PaseoColon Independencia) '(Defensa Venezuela) grafo diccionario)
;(GPS '(PaseoColon Belgrano) '(Defensa Venezuela) grafo diccionario)
;(GPS '(PaseoColon Independencia) '(Defensa Belgrano) grafo diccionario)
(defun GPS (origen destino grafo diccionario)
	(imprimir (traducir (car 
		(GPS2 
			(buscar-nodo origen diccionario) 
			(buscar-nodo destino diccionario) 
			grafo 
			(list (list (buscar-nodo origen diccionario))))) diccionario))
)

;Dado un origen, destino, grafo creamos los posibles caminos para llegar al mismo
;(GPS2 'A 'G '((a (b f c)) (b (g)) (f (g)) (c (a))) '((A)))
;((A F G) (A B G))
(defun GPS2 (origen destino grafo caminos &optional (respuesta nil))
	(if (null (car caminos))
		respuesta
		(GPS3 origen destino grafo caminos respuesta (crear-nuevos-caminos caminos grafo))
	)
)

;-Limpiamos los caminos invalidos de los posibles caminos
;-Agregamos los caminos que llegaron a destino a la respuesta
(defun GPS3 (origen destino grafo caminos respuesta nuevosCaminos)
	(GPS2 origen destino grafo (limpiar-caminos nuevosCaminos origen destino) 
			(append respuesta (caminos-validos nuevosCaminos origen destino)))
)

;Funcion para crear nuevos caminos a partir de las opciones posibles. 
;Si alguna alternativa ya existe en el camino, no se agrega
;Ej: 
;(crear-nuevos-caminos '((A B) (A C)) '((B (C D)) (C (F))) )
;((A B D) (A B C) (A C F))
(defun crear-nuevos-caminos (caminos grafo &optional (respuesta nil))
	(if (null (car caminos))
		respuesta
		(crear-nuevos-caminos (cdr caminos) grafo 
			(append respuesta (crear-caminos (car caminos) (buscar-rel (car (last (car caminos))) grafo)) ))
	)
)

;Creamos nuevos caminos a partir de las relaciones recibidas. 
;Si el alguna rama ya existe en el camino, no la agregamos
;Ej:
;(crear-caminos '(C F) '(X T C) )
;((C F X) (C F T))
(defun crear-caminos (camino relaciones &optional (resultado nil))
	(if (null relaciones)
		resultado
		(if (existe (car relaciones) camino)
			(crear-caminos camino (cdr relaciones) resultado)
			(crear-caminos camino (cdr relaciones) (cons (append camino (list (car relaciones))) resultado))
		)
	)
)

;Sacamos de la lista de caminos los que ya hayan llegado a destino
;(limpiar-caminos '((A B C) (A B D) (A C F)) 'A 'D)
;((A B C) (A C F))
(defun limpiar-caminos (caminos origen destino &optional (respuesta nil))
	(if (null (car caminos))
		respuesta
		(if (llegamos-a-destino (car caminos) origen destino)
			(limpiar-caminos (cdr caminos) origen destino respuesta)
			(limpiar-caminos (cdr caminos) origen destino (append respuesta (list (car caminos))))
		)
	)
)

;Devolvemos una respuesta con los caminos que llegaron a destino
;(caminos-validos '((A) (A B) (A F C G)) 'A 'G)
;((A F C G))
(defun caminos-validos (caminos origen destino &optional (respuesta nil))
	(if (null (car caminos))
		respuesta
		(if (llegamos-a-destino (car caminos) origen destino)
			(caminos-validos (cdr caminos) origen destino (append respuesta (list (car caminos))))
			(caminos-validos (cdr caminos) origen destino respuesta)
		)
	)
)

;Funcion que valida si el origen y destino del camino coinciden con los esperados
;Ej: 
;	camino = (A B C D)
;	origen = A
;	destino = D
;	respuesta = T
(defun llegamos-a-destino (camino origen destino)
	(and (eq origen (car camino)) (eq destino (car (last camino))))
)

;Buscamos las relaciones de algun id en el grafo
;(buscar-rel 'B '( (A (B D)) (B (C D)) ))
;(C D)
(defun buscar-rel (id grafo)
	(cadr (buscar-en-grafo id grafo))
)

;Devolvemos si un id existe en la lista camino
(defun existe (id camino)
	(if (null camino)
		nil
		(if (eq id (car camino))
			T
			(existe id (cdr camino))
		)
	)
)

;Recorremos el grafo hasta encontrar el nodo al cual le corresponde el ID
;Ej: 
;	grafo = ( (A (B D)) (B (C D)) )
;	id = A
;	resultado = (A (B D))
(defun buscar-en-grafo (id grafo)
	(if (any-null id grafo)
		nil
		(if (eq (caar grafo) id)
			(car grafo)
			(buscar-en-grafo id (cdr grafo))
		)
	)
)

(defun verificar-existencia(id grafo)
	(if (null (buscar-en-grafo id grafo))
		nil
		T
	)
)

;Evaluamos si cualquiera de los parametros recibidos es null
(defun any-null(&rest values)
	(reduce (lambda (x y) (or x y)) (mapcar 'null values))
)


;Dada una esquina, buscamos nodo que lo representa en el grafo
;(buscar-nodo '(PaseoColon Independencia) diccionario)
;A
(defun buscar-nodo (esquina diccionario)
	(if (null (car diccionario))
		nil
		(if (and (eq (car esquina) (caadar diccionario)) (eq (car (cdr esquina)) (car (cdadar diccionario))) )
			(caar diccionario)
			(buscar-nodo esquina (cdr diccionario))
		)
	)
)

;Dado un nodo, buscamos la esquina que le corresponde en el diccionario
;(buscar-esquina 'A diccionario)
;(PaseoColon Independencia)
(defun buscar-esquina (nodo diccionario)
	(if (null (car diccionario))
		nil
		(if (eq nodo (caar diccionario)) 
			(cadar diccionario)
			(buscar-esquina nodo (cdr diccionario))
		)
	)
)

;Traducimos un camino descripto por nodos del grafo, a otro
;descripto por los nombres de las esquinas
;(traducir '(A B C) diccionario)
;((PASEOCOLON INDEPENDENCIA) (PASEOCOLON CHILE) (PASEOCOLON MEXICO))
(defun traducir (camino dic &optional (respuesta nil))
	(if (null camino)
		(reverse respuesta)
		(traducir (cdr camino) dic (cons (buscar-esquina (car camino) dic) respuesta))
	)
)

;Devolvemos la cantidad de repeticiones que tiene una calle sobre ese camino
;(cant-rep 'PASEOCOLON '((PASEOCOLON INDEPENDENCIA) (PASEOCOLON CHILE) (DEFENSA MEXICO)) )
;2
(defun cant-rep (calle camino &optional (respuesta -1))
	(if (or (not (or (eq calle (caar camino)) (eq calle (cadar camino)))) (null camino))
		respuesta
		(cant-rep calle (cdr camino) (+ respuesta 1))
	)
)

;Imprimimos por pantalla el recorrido de un camino en particular
;(imprimir-camino '((PASEOCOLON INDEPENDENCIA) (PASEOCOLON CHILE) (DEFENSA MEXICO) (DEFENSA BALCARCE)))
(defun imprimir (camino &optional (sysout nil)) 
	(append (imprimir-camino camino) (list 'hasta 'llegar 'a 'destino))
)

(defun imprimir-camino (camino &optional (sysout nil)) 
	(if (null camino)
		sysout
		(imprimir-camino-recto (caar camino) (cadar camino) camino sysout)
	)
)

;(cant-rep (caar camino) camino) (cant-rep (cadar camino) camino))
(defun imprimir-camino-recto (calle1 calle2 camino sysout)
	(imprimir-camino-recto2 calle1 (cant-rep calle1 camino) calle2 (cant-rep calle2 camino) camino sysout)
)

;(cant-rep (caar camino) camino) (cant-rep (cadar camino) camino))
(defun imprimir-camino-recto2 (calle1 distancia1 calle2 distancia2 camino sysout)
	(if (>  distancia1 distancia2)
		(imprimir-doblar (nthcdr distancia1 camino) (append sysout (imprimir1 calle1 distancia1)))
		(imprimir-doblar (nthcdr distancia2 camino) (append sysout (imprimir1 calle2 distancia2)))
	)
)

(defun imprimir-doblar (camino sysout)
	(if (null (cdr camino))
		sysout
		(imprimir-doblar2 (caar camino) (cadar camino) camino sysout)
	)
)

(defun imprimir-doblar2 (calle1 calle2 camino sysout)
	(if (>  (cant-rep calle1 camino) (cant-rep calle2 camino))
		(imprimir-camino camino (append sysout (imprimir2 calle1)))
		(imprimir-camino camino (append sysout (imprimir2 calle2)))
	)
)

(defun imprimir1 (calle distancia) 	
	(list 'recorrer distancia 'cuadras 'por calle) 
)

(defun imprimir2 (calle) 	
	(list 'y 'doblar 'en calle) 
)