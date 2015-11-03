module String.KataSpec where

import Test.Hspec
import String.Kata

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "calc" $ do
        it "calculates 0 for empty string" $
            calc "" `shouldBe` 0
        it "calculates 0 for string \",\"" $
            calc "," `shouldBe` 0
        it "calculates 3 for string \"1,2\"" $
            calc "1,2" `shouldBe` 3
        it "calculates 10 for string \"1,2,3,4\"" $
            calc "1,2,3,4" `shouldBe` 10
    describe "getUpTo" $ do
        it "separate string \"\" by delimiter ','" $
            getUpTo ("", "") ',' `shouldBe` ("", "")
        it "separate string \"\" by delimiter ','" $
            getUpTo ("", ",") ',' `shouldBe` ("", "")
        it "separate string \"1,2\" by delimiter ','" $
            getUpTo ("", "1,2") ',' `shouldBe` ("1", "2")
        it "separate string \"123,456\" by delimiter ','" $
            getUpTo ("", "123,456") ',' `shouldBe` ("123", "456")
        it "separate string \"123\" by delimiter ','" $
            getUpTo ("", "123") ',' `shouldBe` ("123", "")
    describe "split" $ do
        it "splits empty string" $
            split "" ',' `shouldBe` []
        it "splits two elements" $
            split "1,2" ',' `shouldBe` ["1","2"]
