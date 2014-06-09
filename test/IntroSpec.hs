module IntroSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
    describe "testing the test framework" $ do
        it "works if this test passes" $ do
            True == True `shouldBe` True
