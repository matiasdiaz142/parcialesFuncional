saludable persona = (noObesa persona) && (estaTonificado persona)
esObeso (edad,peso,tonificacion) = peso >100
noObesa (edad,peso,tonificacion) = peso <100
estaTonificado (edad,peso,tonificacion) = tonificacion >5
quemarCalorias (edad,peso,tonificacion) calorias | esObeso (edad,peso,tonificacion) = (edad,peso - (div calorias 150),tonificacion)
												 | (noObesa (edad,peso,tonificacion)) && (edad > 30) && (calorias > 200) = (edad,peso-1,tonificacion)
												 | otherwise = (edad,peso- (div (calorias) (peso*edad)),tonificacion)
												 
caminata tiempo (edad,peso,tonificacion) = quemarCalorias (edad,peso,tonificacion) (5*tiempo)

pesas kilos tiempo (edad,peso,tonificacion) | tiempo > 10 = (edad,peso,tonificacion + div kilos 10)
											| otherwise = (edad,peso,tonificacion)
colina inclinacion tiempo (edad,peso,tonificacion) = quemarCalorias (edad,peso,tonificacion) (2*tiempo*inclinacion)

laPosta (nombre,duracion,lista) persona = rutina (nombre,duracion,(map (agregar duracion lista) lista)) persona
rutina (nombre,duracion,(ejercicio:resto)) persona = rutina (nombre,duracion,resto) (ejercicio persona)
rutina (nombre,duracion,[]) persona = persona
confol (nombre,duracion,lista) persona = foldl (\ a b -> b a) persona (map (agregar duracion lista) lista)
agregar duracion lista funcion = funcion (div duracion (length lista))


toni (_,_,t) = t
kilo (_,k,_) = k
kilosperdidos rutin persona = (kilo persona) - (kilo.rutina rutin) persona 
resumen rutin persona = ((\(a,b,c) -> a) rutin, kilosperdidos rutin persona, (toni.rutina rutin) persona)
nombre lista = map (darlo) lista
darlo (a,_,_) = a
hacenBien lista persona = (nombre.filter (esBuena persona)) lista
esBuena persona rutin = (saludable.rutina rutin) persona

