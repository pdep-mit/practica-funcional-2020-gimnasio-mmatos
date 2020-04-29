import Library
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Precalentamiento" $ do
    it "El ejercicio relax no impacta al gimnasta" $ do
      relax 60 gimnastaDePrueba `shouldBe` gimnastaDePrueba

    describe "Punto 1: Gimnastas saludables" $ do
        it "Un gimnasta que pesa más de 100 kilos es obeso" $ do
            esObeso (Gimnasta {peso = 110, tonificacion = 4, edad = 40}) `shouldBe` True
        it "Un gimnasta que pesa menos de 100 kilos no es obeso" $ do
            esObeso (Gimnasta {peso = 90, tonificacion = 4, edad = 40}) `shouldBe` False
        it "Un gimnasta con tonificación menor a 5 que no es obeso no está saludable" $ do
            estaSaludable (Gimnasta {peso = 90, tonificacion = 4, edad = 30}) `shouldBe` False
        it "Un gimnasta con tonificación mayor a 5 que no es obeso está saludable" $ do
            estaSaludable (Gimnasta {peso = 90, tonificacion = 7, edad = 30}) `shouldBe` True
        it "Un gimnasta con tonificación menor a 5 que es obeso no está saludable" $ do
            estaSaludable (Gimnasta {peso = 120, tonificacion = 3, edad = 30}) `shouldBe` False
        it "Un gimnasta con tonificación mayor a 5 que es obeso no está saludable" $ do
            estaSaludable (Gimnasta {peso = 190, tonificacion = 7, edad = 30}) `shouldBe` False