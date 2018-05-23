data Dia = Dia { anio :: Int, mes :: Int, dia :: Int } deriving Eq
data Invitado = Invitado{
nombre :: String,
calendario :: [ RestriccionParaJuntarse ]
}
cantidadDeDiasDelMes unMes unAnio = 30
--1a
type RestriccionParaJuntarse = Dia -> Bool
--1b
enCualesPuedeJuntarse :: [Dia] -> Invitado -> [Dia]
enCualesPuedeJuntarse dias invitado= filter (puedeIrElDia invitado) dias

puedeIrElDia :: Invitado -> Dia -> Bool
puedeIrElDia invitado diaParaReunirse = all (permiteIrEl diaParaReunirse) (calendario invitado) 

permiteIrEl :: Dia -> RestriccionParaJuntarse -> Bool
permiteIrEl dia restriccion =(not . restriccion) dia
--1c
{-
Gracias a Lazy Evaluation, sería posible empezar a evaluar esto, pero como queremos ver
si "TODAS" las restricciones le permiten ir, seguiría evaluando infinitamente a menos que
alguna determine que NO puede ir.
Así que la única forma en que termine la consulta, sería que para cada uno de los días,
haya al menos una restricción que NO le permita ir, devolviendo una lista de días vacía.
En cualquier otro caso, queda evaluando "infinitamnete" (hasta que nos quedemos sin
memoria).
-}
--2a
tengoUnaCita :: Dia -> RestriccionParaJuntarse
tengoUnaCita diaDeLaCita diaDeLaReunion = diaDeLaCita == diaDeLaReunion
--2b
esFeriado :: [Dia] -> RestriccionParaJuntarse
esFeriado listaDiasFeriado diaDeLaReunion = elem diaDeLaReunion listaDiasFeriado
--2c
podriaIrIndeseable :: Invitado -> RestriccionParaJuntarse
podriaIrIndeseable personaIndeseable diaDeLaReunion = puedeIrElDia personaIndeseable diaDeLaReunion
--2d
esFinDeMes :: RestriccionParaJuntarse
esFinDeMes diaDeLaReunion = cantidadDeDiasDelMes (mes diaDeLaReunion) (anio diaDeLaReunion) -4 < dia diaDeLaReunion
--2e
uhJustoTengoTurnoConElDentista :: RestriccionParaJuntarse
uhJustoTengoTurnoConElDentista _ = True

matias = Invitado "Matias" [tengoUnaCita (Dia 2018 05 23),esFeriado [(Dia 2018 05 23)],podriaIrIndeseable (Invitado "Sofia" []),esFinDeMes,uhJustoTengoTurnoConElDentista]

--3
agendarCita :: Dia -> Invitado -> Invitado
agendarCita dia invitado = invitado{calendario = tengoUnaCita dia:calendario invitado}
--4a
type Reunion = [Dia]
mejorDiaParaJuntarse :: Reunion -> [Invitado] -> Dia
mejorDiaParaJuntarse reunion invitados = maximoSegun (flip cantidadDePersonasQueIrianEl invitados) reunion

cantidadDePersonasQueIrianEl :: Dia -> [Invitado] -> Int
cantidadDePersonasQueIrianEl diaDeLaReunion = length . quienesPuedenJuntarseEl diaDeLaReunion

quienesPuedenJuntarseEl :: Dia -> [Invitado] -> [Invitado]
quienesPuedenJuntarseEl diaDeLaReunion = filter (flip puedeIrElDia diaDeLaReunion)


maximoSegun :: (Num n, Ord n) => (a -> n) -> [a] -> a
maximoSegun criterio = foldl1 (mejorSegun criterio)

mejorSegun :: (Num n, Ord n) => (a -> n) -> a -> a -> a
mejorSegun criterio elemento1 elemento2
         | criterio elemento1 >= criterio elemento2 = elemento1
         | otherwise = elemento2
--4b
confirmarReunion :: Reunion -> [Invitado] -> [Invitado]
confirmarReunion reunion invitados = map (confirmarSiPuedeIrEl (mejorDiaParaJuntarse reunion invitados)) invitados

confirmarSiPuedeIrEl :: Dia -> Invitado -> Invitado
confirmarSiPuedeIrEl diaDeLaReunion invitado 
         | puedeIrElDia invitado diaDeLaReunion = agendarCita diaDeLaReunion invitado
         | otherwise = invitado
--5
maratonDeReuniones :: [Reunion] -> [Invitado] -> [Invitado]
maratonDeReuniones reuniones invitados = foldr confirmarReunion invitados reuniones