bart = ("Bart","Homero",(25,60))
todd = ("Todd","Ned",(15,80))
rafa = ("Rafa","Gorgory",(70,50))
nombre (n,_,_) = n
padre (_,p,_) = p
habilidad (_,_,h) = h

palos = putter : madera : map hierro [1 .. 10]


laguna largo = ((\(UnTiro (v,_,a))-> v>80 && between 10 50 a),(\(UnTiro (v,p,a)) -> (v,p,a `div` largo)))
tunelConRampita = ((\(UnTiro (_,p,a)) -> p>90 && a==0), (\(UnTiro(v,_,a)) -> (UnTiro(v*2,100,0))) )
hoyo = ((\(UnTiro (v,p,a)) -> between 5 20 v && p>95 && a==0), (\(UnTiro(v,_,a)) -> (UnTiro(0,0,0)))) 
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b | f a >= f b = a
		     | otherwise = b


data Tiro = UnTiro (Integer, Integer, Integer) deriving Show

golpe (nombre, padre, (fuerza,presicion)) funcion = funcion (nombre , padre , (fuerza,presicion))

putter (nombre, padre, (fuerza,presicion)) = UnTiro (10,(2*presicion), 0) 
madera (nombre, padre, (fuerza,presicion)) = UnTiro (100,presicion*2, 0)
hierro n (nombre, padre, (fuerza,presicion)) = UnTiro (fuerza*n,div presicion n, n*n)

devolverPrimeraTupla (funcion, _) = funcion
puedesuperar funcion (UnTiro (v,p,a)) = (devolverPrimeraTupla funcion) (UnTiro (v,p,a))

--eee mira esto :
palosUtiles persona obstaculo = filter (elPaloFunca (devolverPrimeraTupla obstaculo) persona ) palos 
elPaloFunca condicion persona palo = condicion (palo persona)

nombresDeLosQuePuedenSuperarTodos listaPersonas listaObstaculos = map (nombre) (personasquecumplen listaObstaculos listaPersonas)
personasquecumplen listaO listaP = filter (cumple listaO) listaP
cumple listaO persona = all (supera persona) listaO
supera persona (condicion,_) = any (condicion) (map (invertir persona) palos)
invertir persona palo = palo persona

cuantosObstaculosSupera tiro lista = length (cuantosObstaculosSuperas tiro lista)
cuantosObstaculosSuperas tiro [] = []
cuantosObstaculosSuperas tiro ((condicion,obstaculo):obstaculos) | (condicion tiro) == True = [obstaculo tiro]:(cuantosObstaculosSuperas (obstaculo tiro) obstaculos)
																 | otherwise = []


palillos persona listaO	= maximum (paloMasUtil persona listaO)											 
paloMasUtil persona listaO = map(superacion persona listaO) palos
superacion persona listaO palo = ((cuantosObstaculosSupera (palo persona) listaO), palo)