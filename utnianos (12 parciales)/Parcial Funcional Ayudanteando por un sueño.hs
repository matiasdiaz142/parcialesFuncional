ayudantes =[ ("martina",[("ordenSuperior",6),("expresionLambda",7),("listasPorComprension",8)]),("juan", [("aplicacionParcial",9), ("listasPorComprension",6), ("sinonimosDeTipos",7)]),("sharon", [("currificación",5), ("aplicacionParcial",8), ("tuplas",9), ("ordenSuperior",8)])]




{- punto 1a -}

cantPuntajeEnAlgunTema lista puntaje = (length.(filter ((any ((puntaje==).snd)).snd))) lista

{- punto 1b -}

fueAprendiendo ayudante = (creciente.(map snd).snd) ayudante

creciente (x:[]) = True
creciente (h:t) = (h < head t) && creciente t

{- punto 2a -}

jurado = [puntajeFranco, puntajeMaithe, puntajeHernan]

puntajeFranco ayudante = (promedio.(map snd).snd) ayudante
promedio lista = (fromIntegral.sum) lista / (fromIntegral.length) lista

puntajeMaithe ayudante 	| ((any (("ordenSuperior"==).fst)).snd) ayudante = 9
								| otherwise = 5
								
puntajeHernan ayudante = (fromIntegral.length.snd) ayudante



puntajesDeAyudante jurado ayudante = [ puntajeJuez ayudante | puntajeJuez <- jurado ]

{- punto 2b -}

sumatoriaPuntajes ayudante = (sum.(puntajesDeAyudante jurado)) ayudante

{- punto 2c -}

esBuenAyudante ayudante = ((all (7<=)).(puntajesDeAyudante jurado)) ayudante

{- punto 3a -}

maximoSegun funcion [x] = x
maximoSegun funcion (h:t) = max (funcion h) (maximoSegun funcion t)

{- 
ayudantes = [ (nombre, [(concepto, puntaje)]) ] 
unAyudante = (nombre, [ (concepto, puntaje) ])
conceptos = [ (concepto, puntaje) ] -}

losTresMejores ayudantes juez tema = [ maximoSegun (length.(filter ((7<=).snd)).snd) ayudantes ]

