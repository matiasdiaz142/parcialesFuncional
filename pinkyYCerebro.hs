--1
data Animal = Animal{
coeficiente :: Coeficiente,
especie :: Especie,
capacidades :: [Capacidad]
}
type Capacidad = String
type Coeficiente = Int
type Especie = String

pinky = Animal 100 "Raton" [] 
cerebro = Animal 30 "Raton" ["Dormir","Comer"] 
--2 Transformaciones
inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior n animal = animal{coeficiente = coeficiente animal + n} 

pinkificar:: Animal -> Animal
pinkificar animal = animal{capacidades = []}

superpoderes :: Animal -> Animal
superpoderes animal
        |esEspecie animal "elefante" = agregarHabilidad animal "no tenerle miedo a los ratones"
        |esEspecie animal "raton" && coeficiente animal > 100 = agregarHabilidad animal "hablar"
        |otherwise = animal

esEspecie :: Animal -> String -> Bool
esEspecie animal unaEspecie = especie animal == unaEspecie

agregarHabilidad :: Animal -> String -> Animal
agregarHabilidad animal habilidad = animal{capacidades = capacidades animal ++ [habilidad]}
--3 Criterios
antropomorfico :: Animal -> Bool
antropomorfico animal = elem "hablar" (capacidades animal)

noTanCuerdo :: Animal -> Bool
noTanCuerdo = (> 2).length.filter pinkiesco.capacidades
--4 Experimentos
data Experimento = Experimento{
transformaciones :: [Transformacion],
criterio :: Criterio
}
type Transformacion = Animal -> Animal
type Criterio = Animal -> Bool

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento animal = (criterio experimento) (foldl (flip ($)) animal (transformaciones experimento))
--experimentoExitoso experimento animal = (criterio experimento) (foldl (\a t -> t a) animal (transformaciones experimento))
--experimentoExitoso experimento animal = (criterio experimento) (foldl (aplicarTransformacion) animal (transformaciones experimento))
--aplicarTransformacion animal transformacion = transformacion animal

--se podia hacer con notacion lambda, pero con el flip $ es lo mismo, o haciendo una funcion aparte. habia 3 formas
consulta = experimentoExitoso (Experimento [pinkificar,inteligenciaSuperior 10,superpoderes] antropomorfico) (Animal 17 "Raton" ["Destruir el mundo","Hacer planes desalmados"])
--se supone que se pondria todo ese choclo en consola, si pones consulta en consola se ve que tira False

--5.1
{-Antes que nada sos libre de molestarme todo el dia para preguntar.
Explicacion solo hecha para mel <3, como viste el punto 5 es el mas dificil
tuve que dividir el 5.1 en 3 funciones jajaja, una que se fije que una capacidad este en una lista (capacidadEnLista)
otra que se fije que alguna capacidad del animal este en la lista de capacidades (tieneAlgunaCapacidad)
y por ultimo juntar todo mediante notacion lambda.
-}
reporte1 :: [Animal] -> [Capacidad] -> Experimento -> [Coeficiente]
reporte1 listaAnimales listaCapacidades experimento = map coeficiente (filter (\animal -> (experimentoExitoso experimento animal) && (tieneAlgunaCapacidad listaCapacidades animal)) listaAnimales)
tieneAlgunaCapacidad listaCapacidades animal = any (capacidadEnLista listaCapacidades) (capacidades animal)
capacidadEnLista listaCapacidades capacidad = elem capacidad listaCapacidades 

--5.2
{-Otro mas para mell <3, es igual al punto anterior solo que con un all que un any (o eso pienso yo),
porque pregunta todos en vez de alguna capacidad.
Obviamente tengo cosas mal, porque se repite codigo, el map y el filter son iguales casi-}
reporte2 :: [Animal] -> [Capacidad] -> Experimento -> [Especie]
reporte2 listaAnimales listaCapacidades experimento = map especie (filter (\animal -> (experimentoExitoso experimento animal) && (tieneTodasLasCapacidades listaCapacidades animal)) listaAnimales)
tieneTodasLasCapacidades listaCapacidades animal = all (capacidadEnLista listaCapacidades) (capacidades animal)

--5.3
reporte3 :: [Animal] -> [Capacidad] -> Experimento -> [[Capacidad]]
reporte3 listaAnimales listaCapacidades experimento = map capacidades (filter (\animal -> (experimentoExitoso experimento animal) && (not.tieneTodasLasCapacidades listaCapacidades) animal) listaAnimales)

--6
animalInfinito = Animal 100 "Gatito" (repeat "Holi mel")
experimentoSi = Experimento [pinkificar,inteligenciaSuperior 10] antropomorfico
experimentoNo = Experimento [inteligenciaSuperior 10,superpoderes] antropomorfico

--probar en consola: experimentoExitoso experimentoSi animalInfinito
--probar en consola: experimentoExitoso experimentoNo animalInfinito

--Bonus
pinkiesco :: String -> Bool
pinkiesco habilidad = ((== "hacer").take 5) habilidad


-- palabras pinkiescas = filter pinkiesco (generateWordsUpTo 5)

--generateWordsUpTo 0 = id
--generateWordsUpTo n = (generateWords n) ++ (generateWords n-1)
--PD: estudia discreta tmb ah