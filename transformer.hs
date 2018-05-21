import Text.Show.Functions

data Autobot = Robot{ nombre :: Nombre, habilidades :: Habilidades, transformacion :: Transformacion } | Vehiculo { nombre :: Nombre, habilidades :: Habilidades } deriving (Show)
instance Eq Autobot where (==) a1 a2 = nombre a1 == nombre a2
data Habilidades = Habilidades {
fuerza :: Int,
velocidad :: Int,
resistencia :: Int
} deriving (Show,Eq)
type Nombre = String
type Transformacion = Habilidades -> Habilidades

optimus = Robot "Optimus Prime" (Habilidades 20 20 10) optimusTransformacion
optimusTransformacion (Habilidades _ v r) = Habilidades 0 (v * 5) (r * 2)

jazz = Robot "Jazz" (Habilidades 8 35 3) jazzTransformacion
jazzTransformacion h = Habilidades 0 (velocidad h * 6) (resistencia h * 3)

wheeljack = Robot "Wheeljack" (Habilidades 11 30 4) wheeljackTransformacion
wheeljackTransformacion h = Habilidades 0 (velocidad h * 4) (resistencia h * 3)

bumblebee = Robot "Bumblebee" (Habilidades 10 33 5) bumblebeeTransformacion
bumblebeeTransformacion h = Habilidades 0 (velocidad h * 4) (resistencia h * 2)

autobots = [ optimus, jazz, wheeljack, bumblebee ]

--1
maximoSegun f v1 v2
    | f v1 v2 >= f v2 v1 = v1 
    | otherwise = v2

--2
transformar (Robot n h t) = Vehiculo n (t h)
--3
velocidadContra auto1 auto2 = max 0 (fuerza (habilidades auto2) - resistencia (habilidades auto1))
--4
elMasRapido auto1 auto2
    | velocidadContra auto1 auto2 > velocidadContra auto2 auto1 = auto1
    | otherwise = auto2
--5a
domina robot1 robot2 = all (\tupla -> (fst tupla) == (uncurry elMasRapido) tupla) [(robot1,robot2),(robot1,transformar robot2),(transformar robot1,robot2),(transformar robot1,transformar robot2)]
domina' robot1 robot2 = all ((== nombre robot1).nombre.uncurry elMasRapido) [(robot1,robot2),(robot1,transformar robot2),(transformar robot1,robot2),(transformar robot1,transformar robot2)]
--5b
losDominaATodos robot listaRobots = all (domina robot) listaRobots
--6a
quienesCumplen condicion = map nombre.filter condicion
--6b

--7
saraza :: (Ord b) => a -> a  -> a -> (a -> a -> b) -> b
saraza x y w z = z w . maximoSegun z y $ x