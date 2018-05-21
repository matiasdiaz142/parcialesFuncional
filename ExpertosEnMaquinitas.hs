data Persona = Persona { 
nombre :: String,
dinero :: Float,
suerte :: Int,
factores :: [(String,Int)]
} deriving(Show)

nico = (Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (Persona "Maiu" 100.0 96 [("inteligencia",55), ("paciencia",50)])

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

--7
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)