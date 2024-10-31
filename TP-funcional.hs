import Text.Show.Functions

type Hechizo = (String, Int)

data Mago = Mago {
    nombre :: String,
    edad :: Int,
    salud :: Int,
    hechizos :: [Hechizo]
} deriving(Show)

lagrimaFenix :: Hechizo -> Mago -> Mago
lagrimaFenix hechizo mago = mago { salud = salud mago + snd hechizo }

sectumSempra :: Mago -> Mago
sectumSempra mago
    | salud mago > 10 = mago { salud = salud mago - 10 }
    | otherwise       = mago { salud = salud mago `div` 2 }

obliviate :: Hechizo -> Mago -> Mago
obliviate hechizo mago = mago { hechizos = drop (snd(hechizo)) (hechizos mago) }

confundus :: Mago -> Mago
confundus mago
    | fst(head(hechizos mago)) == "lagrimaFenix" = lagrimaFenix (head(hechizos mago)) mago
    | fst(head(hechizos mago)) == "sectumSempra" = sectumSempra mago
    | fst(head(hechizos mago)) == "obliviate" = obliviate (head(hechizos mago)) mago 
    | otherwise = error "Error"

poder :: Mago -> Int
poder mago = salud mago + (edad mago * length (hechizos mago))

danio :: Mago -> Hechizo -> Int
danio mago hechizo
    | fst(hechizo) == "lagrimaFenix" = salud (lagrimaFenix hechizo mago) - salud mago
    | fst(hechizo) == "sectumSempra" = salud (sectumSempra mago) - salud mago
    | fst(hechizo) == "obliviate" = salud (obliviate hechizo mago) - salud mago
    | fst(hechizo) == "confundus" = salud (confundus mago) - salud mago

diferenciaDePoder :: Mago -> Mago -> Int
diferenciaDePoder mago1 mago2 = abs (poder mago1 - poder mago2)

type Academia = [Mago]

ahiEstaHagrid :: Academia -> Bool
ahiEstaHagrid [] = False
ahiEstaHagrid (mago:resto) =
    (nombre mago == "Hagrid" && null (hechizos mago)) || ahiEstaHagrid resto

todosLosMagosViejosSonNionios :: Academia -> Bool
todosLosMagosViejosSonNionios academia = all nionio viejos
  where
    viejos = filter (\mago -> edad mago > 16) academia
    nionio mago = length (hechizos mago) > 3 * edad mago


--Esta función retorna el elemento de una lista que tiene el mayor valor en la otra función.

elementoMayorValor :: (a -> Int) -> [a] -> a
elementoMayorValor valor [elemento] = elemento
elementoMayorValor valor (elemento1:elemento2:elementoS)
      | valor elemento1 >= valor elemento2 = elementoMayorValor valor (elemento1:elementoS)
      | otherwise = elementoMayorValor valor (elemento2 : elementoS)

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra mago1 mago2 = elementoMayorValor (\hechizo -> danio mago1 hechizo) (hechizos mago2)

mejorOponente :: Mago -> Academia -> Mago
mejorOponente mago academia = elementoMayorValor (\oponente -> diferenciaDePoder mago oponente) academia



--5
noPuedeGanarle :: Mago -> Mago  -> Bool
noPuedeGanarle mago1 mago2 = salud mago1 == salud()--falta completar
