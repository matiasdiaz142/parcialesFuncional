timmy = UnChico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor, aprenderHabilidades ["asd","asdasd"], serGrosoEnNeedForSpeed]
gato = UnChico "wachin" 18 ["mirar television", "manejar"] [serMayor, serGrosoEnNeedForSpeed, aprenderHabilidades ["asd","enamorar"], serGrosoEnNeedForSpeed]
data Chico = UnChico String Int [String] [Chico->Chico]
instance Show Chico where
	show (UnChico nombre edad habilidades deseos) = "Hola soy "++nombre++" y tengo "++(show edad)++" y puedo hacer: "++ (show (take 10 habilidades))

aprenderHabilidades habilidades (UnChico nombre edad habilidadesDelChico deseos) = (UnChico nombre edad (habilidadesDelChico++habilidades) deseos)
serGrosoEnNeedForSpeed (UnChico nombre edad habilidadesDelChico deseos) = (UnChico nombre edad (habilidadesDelChico++need) deseos)
need = ["Ser Groso en need for speed "++ (show n) | n<- [1..]]
serMayor (UnChico nombre edad habilidadesDelChico deseos) = modificarEdad crecer18 (UnChico nombre edad habilidadesDelChico deseos)
wanda (UnChico nombre edad habilidadesDelChico deseos) = modificarEdad madurar((deseos!!0) (UnChico nombre edad habilidadesDelChico (tail deseos)))
cosmo chico = modificarEdad desmadurar chico
modificarEdad criterio chico = criterio chico
crecer18 (UnChico nombre edad habilidadesDelChico deseos) =  (UnChico nombre 18 habilidadesDelChico deseos)
madurar (UnChico nombre edad habilidadesDelChico deseos) = (UnChico nombre (edad+1) habilidadesDelChico deseos)
desmadurar (UnChico nombre edad habilidadesDelChico deseos) = (UnChico nombre (div edad 2) habilidadesDelChico deseos)
muffinMagico (UnChico nombre edad habilidadesDelChico deseos) = foldl (\persona deseo -> deseo persona) (UnChico nombre edad habilidadesDelChico []) deseos

data Chica = UnaChica String (Chico->Bool)
tieneHabilidad habilidad (UnChico nombre edad habilidadesDelChico deseos) = elem habilidad habilidadesDelChico
esSuperMaduro (UnChico nombre edad habilidadesDelChico deseos) = (edad >=18) && (elem "manejar" habilidadesDelChico)

quienConquistaA (UnaChica nombre condicion) listaPretendientes | any (cumplaRequisito condicion) listaPretendientes = (head.filter (cumplaRequisito condicion)) listaPretendientes
															   | otherwise = last listaPretendientes

cumplaRequisito condicion pretendiente = condicion pretendiente

quienConquistaARecursivo (UnaChica nombre condicion) [unPretendiente] = unPretendiente
quienConquistaARecursivo (UnaChica nombre condicion) (unPretendiente:pretendientes) | condicion unPretendiente = unPretendiente
																					| otherwise = quienConquistaARecursivo (UnaChica nombre condicion) pretendientes

nombre (UnChico nombre _ _ _) = nombre
infractoresDeDaRules listaChicos = (map nombre.filter esInfractor ) listaChicos
esInfractor (UnChico nombre edad habilidadesDelChico deseos) = any (habilidadProhibida (UnChico nombre edad habilidadesDelChico deseos)) deseos
habilidadProhibida (UnChico nombre edad habilidadesDelChico deseos) deseo = tieneHabilidadProhibidaDeLas5(deseo (UnChico nombre edad habilidadesDelChico deseos))
tieneHabilidadProhibidaDeLas5 (UnChico nombre edad habilidadesDelChico deseos) =sonProhibidas (take 5 habilidadesDelChico)
sonProhibidas habilidades = (elem "enamorar" habilidades) || (elem "matar" habilidades) || (elem "dominar al mundo" habilidades)