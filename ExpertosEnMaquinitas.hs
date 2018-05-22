data Persona = Persona { 
nombre :: String,
dinero :: Float,
suerte :: Int,
factores :: [(String,Int)]
} deriving(Show)

nico = (Persona "Nico" 100.0 100 [("amuleto", 3), ("manos magicas",100)])
maiu = (Persona "Maiu" 100.0 70 [("inteligencia",55), ("paciencia",50)])

--1
suerteTotal (Persona _ _ suerte lista) 
          | (sum ((map (esAmuleto suerte)) lista)) > 0 = (sum ((map (esAmuleto suerte)) lista))
          | otherwise = suerte
esAmuleto suerte (nombre,factor) 
          | nombre == "amuleto" && factor >= 0 = suerte*factor 
          | otherwise = 0
--2
data Juego = Juego{
nombreJuego :: String,
funcion :: Float -> Float,
criterios :: [Persona -> Bool]
}
ruleta = Juego "ruleta" (*37) [((>80).suerteTotal)]
maquinita = Juego "maquinita" (+jackpot) [((>95).suerteTotal),(tienePaciencia)]
jackpot = 5 
tienePaciencia persona = elem "paciencia" (map fst (factores persona))
--3
puedeGanarJuego persona juego = all (\j -> j persona) (criterios juego)
--4
dineroQuePuedeConseguir jugador apuesta = foldl (\a j ->(funcion j) a) apuesta.filter (puedeGanarJuego jugador)
dineroQuePuedeConseguir2 _ apuesta [] = apuesta
dineroQuePuedeConseguir2 jugador apuesta (juego:xs)
         | puedeGanarJuego jugador juego = (funcion juego) apuesta
         | otherwise = dineroQuePuedeConseguir2 jugador apuesta xs
--5
nombresQueNoPuedenGanar listaJuegos = map nombre.filter (puedeGanar listaJuegos)
puedeGanar :: [Juego] -> Persona -> Bool
puedeGanar juegos jugador = all (not.puedeGanarJuego jugador) juegos
--6
apostar apuesta juego jugador = saldo (jugador{dinero = dinero jugador - apuesta}) juego
saldo :: Persona -> Juego -> Persona
saldo jugador juego
     | puedeGanarJuego jugador juego = jugador{dinero = dinero jugador + (dineroQuePuedeConseguir jugador (dinero jugador) [juego])}
     | otherwise = jugador
--7
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)