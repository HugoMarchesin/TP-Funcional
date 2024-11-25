module Test where
import Prelude
import Test.Hspec
import TPFuncional

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test punto 1" $ do
    it "lanzarLagrimaFenix aumenta la salud del Mago" $ do
      salud (lanzarLagrimaFenix 40 mago1) `shouldBe` 120
    it "sectumSempra le hace daño al mago sobre el que se lanza" $ do
      salud (sectumSempra mago2) `shouldBe` 20
    it "sectumSempra le quita la mitad de la vida" $ do
      salud (sectumSempra mago4) `shouldBe` 4
    it "lanzarObliviate olvida los primeros N hechizos que conozca" $ do
      length (hechizos (lanzarObliviate 2 mago4)) `shouldBe` 1
    it "confundus hace que el mago se ataque a si mismo" $ do
      salud (confundus mago1) `shouldBe` salud(lanzarLagrimaFenix 20 mago1)

  describe "Test punto 2" $ do
    it "El poder de un mago es su salud sumada al resultado de multiplicar su edad por la cantidad de hechizos que conoce" $ do
      poder mago2 `shouldBe` 830
    it "danio da la cantidad de vida que un mago pierde si le lanzan dicho hechizo" $ do
      danio mago3 sectumSempra `shouldBe` -10
    it "La diferencia de poder entre dos magos es el valor absoluto de la resta del poder de cada uno" $ do
      diferenciaDePoder mago1 mago4 `shouldBe` 327

  describe "Test punto 3" $ do
    it "ahiEsta permite saber si hay algún mago sin hechizos cuyo nombre sea Hagrid" $ do
      ahiEsta "Hagrid" 0 academia `shouldBe` True
    it "todosLosMagosViejosSonNionios False" $ do
      todosLosMagosViejosSonNionios academia `shouldBe` False
    it "todosLosMagosViejosSonNionios True" $ do
      todosLosMagosViejosSonNionios [] `shouldBe` True

  describe "Test punto 4" $ do
    it "mejorHechizoContra" $ do
      danio mago1 (mejorHechizoContra mago1 mago2) `shouldBe` -10
    it "mejorOponente" $ do
      nombre (mejorOponente mago2 academia) `shouldBe` "Hagrid"

  describe "Test punto 5" $ do
    it "noPuedeGanarle True" $ do
      noPuedeGanarle mago2 mago5 `shouldBe` True
    it "noPuedeGanarle False" $ do
      noPuedeGanarle mago5 mago2 `shouldBe` False