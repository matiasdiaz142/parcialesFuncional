funcion lista = map (tuplear) lista
tuplear nro = ("lalalal", nro)
hola [x] = x
lala = [10,11,12,13]

pajeracion (x:xs) | (div ((head xs) * 100) x)-100 < 20 = pajeracion xs
				  | otherwise = x
pajeracion [x] = x