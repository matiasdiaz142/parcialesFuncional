data Persona = CPersona String Float Int [(String,Int)] deriving(Show)
nico = (CPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (CPersona "Maiu" 100.0 200 [("inteligencia",55), ("paciencia",50)])
suerteTotal (CPersona _ _ suerte lista) | length(filter(amuletos) lista) == 0 = suerte
										| otherwise = sum(map (esAmuleto suerte) lista)
amuletos (nombre, _) = nombre == "amuleto"
esAmuleto suerte (nombre, factor) | nombre == "amuleto" && factor > 0 = suerte*factor
								  | nombre == "amuleto" && factor == 0 = suerte
								  | otherwise = 0

data Juego = UnJuego String (Float->Float) [Persona->Bool]
instance Show Juego where
	show (UnJuego nombre recompensa lista)= nombre
ruleta = UnJuego "ruleta" (*37) ([(>80).suerteTotal])
maquinita = UnJuego "maquinita" (+jackpot) [((>95).suerteTotal), tienePaciencia]
jackpot = 5
tienePaciencia (CPersona _ _ _ lista) = any (paci) lista
paci (nombre, _) = "paciencia" ==nombre

puedeGanar persona (UnJuego _ _ lista) =  all (inversion persona) lista
inversion persona criterio = criterio persona
ganancia apuesta (UnJuego _ funcion _) = funcion apuesta

cantidad (nombre, ganancia) = ganancia
loQueGano persona apuesta listaJuegos = cantidad ((foldl (puedeGanarPio) (persona, apuesta) listaJuegos))
puedeGanarPio (persona, apuesta) juego | (puedeGanar persona juego) = (persona, (ganancia apuesta juego))
									   | otherwise = (persona, apuesta)

loQueGanoRecursiva persona apuesta [] = apuesta
loQueGanoRecursiva persona apuesta (juego:juegos) | (puedeGanar persona juego) = loQueGanoRecursiva persona (ganancia apuesta juego) juegos
												  | otherwise =loQueGanoRecursiva persona apuesta juegos
losperdidos listaP listaJ = filter (noganan listaJ) listaP
noganan listaJ persona = all (nogananada persona) listaJ
nogananada persona juego = not(puedeGanar persona juego)

saldo (CPersona nombre dinero suerte lista) apuesta juego | puedeGanar (CPersona nombre dinero suerte lista) juego = (CPersona nombre (dinero - apuesta + (ganancia apuesta juego)) suerte lista)
														  | otherwise = (CPersona nombre (dinero-apuesta) suerte lista)

														  
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)
