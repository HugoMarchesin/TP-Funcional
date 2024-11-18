import Text.Show.Functions

data Mago = Mago {
    nombre :: String,
    edad :: Int,
    salud :: Int,
    hechizos :: [Hechizo]
} deriving ( Show)

type Hechizo = Mago -> Mago

lanzarLagrimaFenix :: Int -> Hechizo
lanzarLagrimaFenix cantidad mago = mago { salud = salud mago + cantidad }

sectumSempra :: Hechizo
sectumSempra mago
    | salud mago > 10 = mago { salud = salud mago - 10 }
    | otherwise       = mago { salud = salud mago `div` 2 }

lanzarObliviate :: Int -> Hechizo
lanzarObliviate n mago = mago { hechizos = drop n (hechizos mago) }

confundus :: Hechizo
confundus mago = (head (hechizos mago)) mago





--Item 2

poder :: Mago -> Int
poder mago = salud mago + (edad mago * length (hechizos mago))

danio :: Mago -> Hechizo -> Int
danio mago hechizo = (salud (hechizo mago) ) - salud mago

diferenciaDePoder :: Mago -> Mago -> Int
diferenciaDePoder mago1 mago2 = abs (poder mago1 - poder mago2)





--Item 3

type Academia = [Mago]

ahiEsta :: String -> Int -> Academia -> Bool
ahiEsta nombreMago cantHechizos academia = any (\ mago -> and [nombre mago == nombreMago, length (hechizos mago) == cantHechizos]) academia

todosLosMagosViejosSonNionios :: Academia -> Bool
todosLosMagosViejosSonNionios academia = all nionio viejos
  where
    viejos = filter (\mago -> edad mago > 16) academia
    nionio mago = length (hechizos mago) > 3 * edad mago




--Item 4

--Esta función retorna el elemento de una lista que tiene el mayor valor en la otra función.

elementoMayorValor :: (a -> Int) -> [a] -> a
elementoMayorValor valor [elemento] = elemento
elementoMayorValor valor (elemento1:elemento2:elementoS)
      | valor elemento1 >= valor elemento2 = elementoMayorValor valor (elemento1:elementoS)
      | otherwise = elementoMayorValor valor (elemento2 : elementoS)

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra mago1 mago2 = elementoMayorValor (danio mago1) (hechizos mago2)

mejorOponente :: Mago -> Academia -> Mago
mejorOponente mago academia = elementoMayorValor (diferenciaDePoder mago) academia




--Item 5

noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago1 mago2 =
    foldl1 (+) (map (danio mago1) (hechizos mago2)) == 0