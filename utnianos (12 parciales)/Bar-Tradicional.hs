data Cafe = UnCafe String Int
data Gaseosa = UnaGaseosa String Int
data Helado = UnHelado (String,String) String deriving(Show)
data Agua = UnAgua String
class Bebida a where
	calorias::a->Int
	contenidoCalorico::a->String
	contenidoCalorico bebida | calorias(bebida) < 100 = "Bajo"
							 | calorias(bebida) > 100 && calorias(bebida) < 200 = "Medio"
							 | otherwise = "Alto"
	esEnergizante::a->Bool
	esRecomendable::a->Bool
	esRecomendable bebida | esEnergizante(bebida) && contenidoCalorico(bebida) == "Bajo" = True
						  | otherwise = False

instance Bebida Cafe where
	calorias (UnCafe _ azucar) = 3*azucar
	esEnergizante (UnCafe nombre _) | nombre == "capuchino" = True
									| otherwise = False

instance Bebida Gaseosa where
	calorias (UnaGaseosa _ azucar) = azucar
	esEnergizante (UnaGaseosa nombre _) | nombre == "pomelo" = True
										| otherwise = False

instance Bebida Helado where
	calorias (UnHelado (gusto1, gusto2) baniado) | baniado == "chocolate" && (gusto1 == "chocolate" || gusto2 == "chocolate") = 370
												 | otherwise = 220
	esEnergizante (UnHelado (gusto1, gusto2) _) | gusto1 == "chocolate" || gusto2 == "chocolate" = True
												| otherwise = False
	
instance Bebida Agua where
	calorias (UnAgua _) = 0
	esEnergizante bebida = False
	
preparar gusto1 gusto2 baniado = UnHelado (gusto1,gusto2) baniado
	