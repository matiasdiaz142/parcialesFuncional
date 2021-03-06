data Usuario = Usuario{
nombre :: Nombre,
seguidos :: [Nombre]
}
usuarios = [ Usuario "@marsupialRengo" ["@don_churrasco", "@dodainOk"], Usuario "@don_churrasco" [], Usuario "@lapipi" ["@dodainOk"], Usuario "@dodainOk" ["@lapipi", "@marsupialRengo"] ]

data Mensaje = Mensaje{
usuario :: Nombre,
texto :: Texto,
repips :: Cantidad,
favs :: Cantidad 
}

type Nombre = String
type Texto = String
type Cantidad = Int
mensajes = [ Mensaje "@dodainOk" "Las personas que viajan en tren se quejan de llenos" 20 100, Mensaje "@lapipi" "En mi mundo todos son un pony" 1 0, Mensaje "@lapipi" "y comen arcoiris y su popo son mariposas" 0 0, Mensaje "@dodainOk" "No hay problema en cometer errores, el secreto es no pasarlos a produccion" 1 3]
quickSortBy f [] = []
quickSortBy f (x:xs) =
    sublistaQueCumple ((< f x).f) ++ [x] ++ sublistaQueCumple ((>= f x).f)
    where sublistaQueCumple condicion = quickSortBy f . filter condicion $ xs

find f = head.filter f

--1
sigueA seguidor seguido = elem seguido.seguidos.find((==seguidor).nombre) $ usuarios

--2
seguidores seguido = map nombre.filter ((flip sigueA seguido).nombre) $ usuarios
--3
trencitoDeUsuarios [_] = True
trencitoDeUsuarios (u1:u2:us) = sigueA u1 u2 && trencitoDeUsuarios (u2:us)
--4a
favear mensaje = mensaje{favs = favs mensaje + 1}
--4b
repipear mensaje usuario1 = Mensaje usuario1 (usuario mensaje ++ ": " ++ texto mensaje) 0 0
--4c
repipearTodos mensajes usuario = map (\m -> m{repips = repips m + 1}) mensajes
--61
loMasAlgo f lista = take 3.reverse.(quickSortBy f ) $ lista
--62a
loMasRepipeados pips = loMasAlgo repips pips
--62b
loMasGrandesPips = loMasAlgo (length.texto)
--62c
loMasPipeadores = loMasAlgo (length.mensajesPipeados)
 where mensajesPipeados u = filter ((==u).usuario) mensajes