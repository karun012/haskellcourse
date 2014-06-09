import Test.DocTest

main = doctest["-isrc", "src/intro.hs", "src/golf.hs", "src/logAnalysis.hs", "src/polymorphismAndTypeClasses.hs"]
