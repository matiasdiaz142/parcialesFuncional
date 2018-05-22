data Persona = UnaPersona [String] Bool deriving (Show)

data PowerRangers = UnPower String [String] Int deriving (Show)
elAzul = UnPower "Azul" ["ads", "asdads", "asddsa"] 50
convertirEnPowerRanger (UnaPersona lista _) color = UnPower color (map (agregarSuper) lista) (sum(map(cantLetras) lista))
agregarSuper habilidad = "super" ++ habilidad
cantLetras habilidad = length habilidad 

formarEquipoRanger listaPersonas listacolores = zipWith convertirEnPowerRanger (filter esBueno listaPersonas) listacolores 
esBueno (UnaPersona _ bondad) = bondad 

find0rElse condicion valor lista |length(filter (condicion) lista) >0 =head(filter (condicion) lista)
								 | otherwise = valor

rangerLider lista | length(filter(esRojo)lista) > 0 = head(filter(esRojo)lista)
				  | otherwise = head(lista)

esRojo (UnPower color _ _) = color == "Rojo"
maximumBy [x] funcion = x
maximumBy (x:xs) funcion | funcion x > funcion (head(xs)) = maximumBy (x:tail(xs)) funcion
						 | otherwise = maximumBy xs funcion
						 
rangerMasPoderoso lista = maximumBy lista transformar
transformar (UnPower _ _ pelea) = pelea  

rangerHabilidoso (UnPower _ lista _ )= length(lista) > 5
inf algo = algo ++ inf algo
alfa5 = UnPower "Metalico" ["reparar cosas", inf "ay "] 0