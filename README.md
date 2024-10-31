mago1 :: Mago
mago1 = Mago "Gandulfo" 300 80 [("lagrimaFenix", 20), ("sectumSempra", 0)]

mago2 :: Mago
mago2 = Mago "Saruman" 400 30 [("sectumSempra", 0), ("obliviate", 1)]

mago3 :: Mago
mago3 = Mago "Hagrid" 30 50 []

mago4 :: Mago
mago4 = Mago "Chispitas" 115 70 [("confundus", 0), ("lagrimaFenix", 15), ("sectumSempra", 0)]

academia :: Academia
academia = [mago1, mago2, mago3, mago4]

poderMago1 = poder mago1  -- Debería ser 80 + (300 * 2) = 680

danioMago2AMago1 = danio mago1 ("sectumSempra", 0)  -- Debería ser 10

diferencia = diferenciaDePoder mago1 mago2  -- Diferencia en poder

mejorHechizo = mejorHechizoContra mago1 mago2

mejorOponenteMago1 = mejorOponente mago1 academia

resultadoNoPuedeGanarle = noPuedeGanarle mago1 mago2  -- Debería ser True o False

runTests  :: int -> String
runTests test
    | poderMago1 /= 680 = error "Error"
    | danioMago2AMago1 /= -10 = error "Error"
    | diferencia /= 150 = error "Error"
    | mejorHechizo /= ("obliviate", 1) = error "Error"
    | resultadoNoPuedeGanarle /= False = error "Error"
    | ahiEstaHagrid [mago1, mago2, mago3, mago4] /= True = error "Error"
    | otherwise = "OK"
