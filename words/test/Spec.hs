import Test.Hspec
import Lib
main :: IO ()
main = hspec $ do
  describe "how to write a test" $ do
    it "someString retuan a String" $ do
      someString `shouldBe` "someString"
