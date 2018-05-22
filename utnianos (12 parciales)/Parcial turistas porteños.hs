lugaresDeEjemplo = [("Rosario", 300,["Monumento a la bandera", "Rio","Cigarrillos"]),("Mar Del Plata", 400, ["Playa", "Alfajores", "Puloveres"]), ("Bariloche",1600, ["Montanas", "Nieve", "Puloveres", "Chocolate"])]
juan =("Juan", playero)
ana = ("Ana", mejorCerca)
jorge = ("Jorge", gastronomico)
zulma = ("Zulma", playero)
fumancha = ("Fumanchero", (\(_,_,caracteristicas)-> elem "Cigarrillos" caracteristicas))

esComestible producto = (producto == "Chocolate") || (producto == "Alfajores")
playero (_,_,caracteristicas)= elem "Playa" caracteristicas
mejorCerca (_,distancia,_) = distancia <= 500
gastronomico (_,_,caracteristicas) = any (esComestible) caracteristicas
esquiador (_,distancia,caracteristicas) = (distancia >=500) && (elem "Nieve" caracteristicas) && (elem "Montana" caracteristicas)

puedeIr persona listaLugares = (map nombre.filter (leGusta persona)) listaLugares
leGusta (_, criterio) lugar = criterio lugar
nombre (n,_,_) = n

maximumBy criterio [x] = x
maximumBy criterio (x:xs) | criterio x > criterio (head xs) = maximumBy criterio (x:(tail xs))
						  | otherwise = maximumBy criterio xs

nombreMiTupla (_,n) = n
lugarMasElegido listaPersonas listaLugares = (nombreMiTupla.maximumBy (\(a,b) -> a).map (calcularYDarNombre listaPersonas)) listaLugares
calcularYDarNombre listaPersonas lugar = ((length.filter (quierenIr lugar)) listaPersonas, nombre lugar )
quierenIr lugar (nombre, criterio) = criterio lugar

puedenIrTodosA listaP lugar = all (puedeIrBis lugar) listaP
puedeIrBis lugar (_,criterio) = criterio lugar

lugaresFamilieros familia lugares = (map nombre.filter (puedenIrTodosA familia)) lugares
