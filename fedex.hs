-- Parcial Paradigmas de Programacion -- FedeX -- Sabados Mañana -- 13/05/2017
--1
data Envio = Envio{
origen :: Lugar,
destino :: Lugar,
precio :: Float,
peso :: Float,
categorias :: [Categoria],
impuestos :: [Impuesto]
}

type Categoria = String
type Lugar = (String,String)

ciudad = fst
pais = snd

--Impuestos
iva = (*0.2).precio
extranio = valorSegun (even.round.precio) ((*0.01).precio)
multicategorico = valorSegun ((>3).length.categorias) ((*0.01).precio)

--Cargos

--aduanero = valorSegun esInternacional ((*0.03).precio)
cargoArbitrario envio = envio{precio = precio envio + 50}

--Funcion auxiliar para evitar repetir codigo en Impuestos y Cargos
valorSegun condicion calculo envio 
    | condicion envio = calculo envio
    | otherwise = 0

--1a
type Impuesto = Envio -> Float
--1b
type Cargo = Envio -> Envio

--2a
--cargoCategorico categoria porcentaje envio = Envio{precio = precio envio + v}
--    where v = valorSegun (elem categoria (categorias)) ((*porcentaje).(/100).precio)

--cargoTecnologico = cargoCategorico "Tecnologia" 18

--2b
envioInternacional = Envio ("Buenos Aires","Argentina") ("Utrecht","Paises Bajos") 220 2 ["Musica","Tecnologia"] []
envioNacional = Envio ("California","Estados Unidos") ("Miami","Estados Unidos") 1500 5 ["Libros"] [iva,extranio]

--3a 
cuestaMas valor = (>valor).precio
--3b
esBarato = not.cuestaMas 1300
--4a
seDirige paisDirige = (==paisDirige).pais.destino
--4b
esLocal envio = seDirige (pais.origen $ envio) envio
esInternacional = not.esLocal
--5
enviosDeCategorias categorias' envios = filter (flip all categorias'.flip elem.categorias) envios
--6
precioFinal cargos envio = aplicarImpuestos $ foldl (flip ($)) envio cargos

aplicarImpuestos envio = foldl (\total impuesto -> total + impuesto envio)(precio envio) (impuestos envio)