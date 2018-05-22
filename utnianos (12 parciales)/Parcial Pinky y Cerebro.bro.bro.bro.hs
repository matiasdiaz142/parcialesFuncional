data Animal = UnAnimal Int String [String] deriving (Show)
perro = UnAnimal 50 "Perro" ["Vuelaasdds","Ladraasds","Falopaasdsda"]
inteligenciaSuperior n (UnAnimal coef especie habilidades) = (UnAnimal (coef+n) especie habilidades)
pinkificar (UnAnimal coef especie _) = (UnAnimal coef especie [])
superpoderes (UnAnimal coef especie habilidades) | especie == "elefante" = (UnAnimal coef especie ("No tenerlo miedo a ls ratones":habilidades))
												 | especie == "raton" && coef > 100 = (UnAnimal coef especie ("puede hablar":habilidades))
												 | otherwise = (UnAnimal coef especie habilidades)
												 
antropomorfico (UnAnimal coef especie habilidades) = coef>60 && (elem "puede hablar" habilidades)
noTanCuerdo (UnAnimal coef especie habilidades) = ((length.filter (pinkiesco)) habilidades) > 2

data Experimento = UnExperimento [Animal->Animal] (Animal->Bool)
transformadadeLaplace = UnExperimento [inteligenciaSuperior 10, pinkificar, superpoderes] antropomorfico
experimentoExitoso (UnExperimento lista criterio) animal = criterio(foldl (\a b->b a) animal lista)
realizarTransformaciones (transformacion:transformaciones) animal = realizarTransformaciones transformaciones (transformacion animal)
realizarTransformaciones [] animal = animal
raton = UnAnimal 17 "raton" ["destruir el mundo","Hacer planes desalmados"]
locura = UnExperimento [pinkificar, inteligenciaSuperior 10, superpoderes] antropomorfico
experimentoExitoso locura raton

darCoef (UnAnimal coef _ _ ) = coef
informe1 animales capacidades (UnExperimento transformaciones _)= informe animales capacidades (UnExperimento transformaciones _) darCoef esCapaz
informe animales capacidades (UnExperimento transformaciones _) criterioMostrar criterioFiltrar = map (criterioMostrar)(filter(criterioFiltrar capacidades transformaciones) animales)
esCapaz capacidades transformaciones animal =any (tieneCapacidades (realizarTransformaciones transformaciones animal)) capacidades
tieneCapacidades (UnAnimal _ _ habilidades) capacidad = elem capacidad habilidades 

darEspecie (UnAnimal _ especie _) = especie
informe2 animales capacidades (UnExperimento transformaciones _) = informe animales capacidades (UnExperimento transformaciones _) darEspecie esReCapaz
esReCapaz capacidades transformaciones animal = all (tieneCapacidades (realizarTransformaciones transformaciones animal)) capacidades

darCantCapacidades (UnAnimal _ _ habilidades) = length habilidades
informe3 animales capacidades (UnExperimento transformaciones _) = informe animales capacidades (UnExperimento transformaciones _) darCantCapacidades noEsCapaz
noEsCapaz capacidades transformaciones animal = not(esCapaz capacidades transformaciones animal)

-- Todos los experimentos se pueden realizar, excepto alguna con el criterio NoTanCuerdo ya que estaria filtrando infinitamente la lista de habilidades y nunca devolveria el valor True o False para realizar el experimento
--Bonus

pinkiesco habilidad = ((take 6 habilidad) == "hacer ") && (isVowel habilidad!!7 || isVowel habilidad!!8 || isVowel habilidad!!9 || isVowel habilidad!!10)

palabraspinkiescas = map (meterleHacer) ((filter (isVowel habilidad!!7 || isVowel habilidad!!8 || isVowel habilidad!!9 || isVowel habilidad!!10)) generateWorldsUpTo 4)
meterleHacer cadena = "Hacer "++cadena

generateWorldsUpTo n = generateWorlds n++generateWorldsUpTo n
