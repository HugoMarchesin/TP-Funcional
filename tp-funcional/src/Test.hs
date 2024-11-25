module Test where
import Prelude
import Test.Hspec
import TPFuncional

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test para los hechizos" $ do
    it "LagrimaFenix aumenta la salud del Mago" $ do
      salud (lanzarLagrimaFenix 40 mago1) `shouldBe` 120