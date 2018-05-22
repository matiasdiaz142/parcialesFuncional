propuestasGHC = [("franco", "detectorDeParcialesCopiados", ["definicionDeLenguajes", "parsing", "compiladores"],3),("adriel", "entornoDeProgramacionHechoEnHaskell", ["programacionFuncional", "monadas", "tiposDeDatosPropios"],2)]
aniosDeExperiencia (_,_,_,anios) = anios
skills (_,_,sks,_) = sks
mentoresGHC = [("carlono", ["detectorDeParcialesCopiados","mejorarPerformanceDelMotor"], (\propuesta -> (length.skills) propuesta + (aniosDeExperiencia propuesta))),("nicolas",["entornoDeProgramacionHechoEnHaskell", "extensionesDelLenguaje"],((3+).aniosDeExperiencia))]
puntosSegun propuesta mentor | (estaInteresado propuesta mentor) = ((+1).votacionDelMentor mentor) propuesta
							 | otherwise = (votacionDelMentor mentor propuesta)

estaInteresado (_,proyecto,_,_) (_,intereses,_) = elem proyecto intereses
votacionDelMentor (_,_,criterio) propuesta = criterio propuesta

puntajeTotal listaMentores propuesta = (sum.map (puntosSegun propuesta)) listaMentores

propuestasConChances listaPropuestas = filter (tieneChances) listaPropuestas
tieneChances (_,_,skl,_) = length skl >= 3
nombre (n,_,_,_) = n
proyecto (_,p,_,_) = p
ranking listaPropuestas listaMentores = map (puntajeTotalBis listaMentores) listaPropuestas
puntajeTotalBis listaMentores propuesta = (nombre propuesta, proyecto propuesta, (puntajeTotal listaMentores propuesta))

propuestasDeInteres mentor listaPropuestas = filter (leInteresa mentor) listaPropuestas
leInteresa (_,int,_) (_,tema,_,_) = elem tema int 

maximumBy criterio [x] = x
maximumBy criterio (x:xs) | criterio x > criterio (head xs) = maximumBy criterio (x:(tail xs))
						  | otherwise = maximumBy criterio xs
puntaje (_,_,puntos) = puntos
resultadoConMasVotos listaPropuestas listaMentores = maximumBy puntaje (ranking listaPropuestas listaMentores)

nombreM (n,_,_) = n
nombreMentorMasInteresado propuesta listaMentores = ((\(nombre,puntaje)->nombre).maximumBy (\(nombre, puntaje)-> puntaje).map (darPuntosyNombre propuesta)) listaMentores
darPuntosyNombre propuesta mentor = (nombreM mentor, puntosSegun propuesta mentor)

proyectosElegidos listaMentores listaPropuestas = (map (armarTupla listaMentores).filter (elegidos listaMentores)) listaPropuestas
elegidos listaMentores propuesta = puntajeTotal listaMentores propuesta > 12
armarTupla listaMentores propuesta = (nombre propuesta, nombreMentorMasInteresado propuesta listaMentores, proyecto propuesta)