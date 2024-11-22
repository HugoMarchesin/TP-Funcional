module Spec where
import TP-funcional
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test para los hechizos" $ do
    it "LagrimaFenix aumenta la salud del Mago" $ do
      lagrimaFenix 40 mago1 `shouldBe` Mago "Gandulfo" 300 120 [lanzarLagrimaFenix 20, sectumSempra]
