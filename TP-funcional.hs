{-Enunciado
En el mundo mágico, los magos se pelean entre sí y se lanzan hechizos de aquí por allá. Para poner orden en la escuela de magia, vamos a crear un sistema que nos permita realizar estos duelos de forma controlada.
Los magos sabemos que tienen un nombre, una edad, una cantidad de salud y un conjunto de hechizos. Con un hechizo un mago puede hacerle cambios a otros magos.
Se pide resolver los siguientes puntos, aprovechando al máximo los conceptos del paradigma funcional:

1- Elegir un tipo de dato con el que representar a los Magos y los Hechizos, pensando que debe modelar los siguientes hechizos (estos ejemplos son solamente una muestra de todos los hechizos que puede hacer un mago, por
lo cual debe ser una solución que permita agregar futuros hechizos):
    a-lagrimaFenix: Este hechizo recibe un mago y el mismo recupera una cierta cantidad de salud. Este hechizo puede usarse para curar distintas cantidades de vida.
    b-sectumSempra: Este hechizo le hace daño al mago sobre el que se lanza. Si la salud de dicho mago es mayor a 10, le hace 10 puntos de daño, de lo contrario le quita la mitad de su vida actual.
    c-obliviate: El mago que recibe por parámetro olvida los primeros N hechizos que conozca. Se puede lanzar este hechizo con diferentes valores de N.
    d-confundus: El mago objetivo se ataca a sí mismo con su primer hechizo de su lista de hechizos. Puede lanzar error si no tiene hechizos.

2- Modelar las siguientes funciones respetando los tipos pedidos:
    a- poder :: Mago -> Int
    El poder de un mago es su salud sumada al resultado de multiplicar su edad por la cantidad de hechizos que conoce.
    b- daño :: Mago -> Hechizo -> Int
    Esta función retorna la cantidad de vida que un mago pierde si le lanzan dicho hechizo. Si gana vida, la misma debe reflejarse como negativa. Si no pierde o gana vida, está bien que devuelva 0.
    c- diferenciaDePoder :: Mago -> Mago -> Int
    La diferencia de poder entre dos magos es el valor absoluto de la resta del poder de cada uno. Esto siempre retorna un número positivo.
3- Dada una Academia, la cual representamos con el siguiente tipo de dato:
    type Academia = [Mago]
    Se pide escribir el código necesario para realizar las siguientes consultas:
    a- Saber si hay algún mago sin hechizos cuyo nombre sea “Hagrid”.
    b- Saber si todos los magos viejos (cuya edad sea mayor a 16) son ñoños. Esto ocurre si tienen más hechizos que el triple de su edad.
4- Dada la siguiente función:
    f x [y] = y
    f x (y1:y2:ys)
        | x y1 >= x y2 = f x (y1:ys)
        | otherwise = f x (y2 : ys)
    Se pide:
    a- Describir brevemente para qué sirve, explicitar su tipo y mejorarla en términos de Expresividad.
    b- Usar esta función para definir las siguientes funciones, sin definir funciones auxiliares:
        a- mejorHechizoContra :: Mago -> Mago -> Hechizo
        Dados dos magos, retorna el hechizo de la lista de hechizos del segundo mago que le haga más daño al primero.
        b-mejorOponente :: Mago -> Academia -> Mago
        Dado un mago y una academia, retorna el mago de la academia que tenga la mayor diferencia de poder con el mago recibido.
5- Definir la siguiente función sin utilizar recursividad:
    noPuedeGanarle :: Mago -> Mago  -> Bool
    Decimos que el segundo mago no puede ganarle al primero si, luego de hechizarlo con todos los hechizos que conoce (uno atrás del otro) la salud del primer mago sigue siendo la misma.
    -}

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