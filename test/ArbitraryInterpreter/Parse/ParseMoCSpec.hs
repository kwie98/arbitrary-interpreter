module ArbitraryInterpreter.Parse.ParseMoCSpec
( spec
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseMoC
import Control.Exception (evaluate)
import Data.Maybe (isJust, isNothing)
import Test.Hspec


spec :: Spec
spec = do
    describe "parseMoC" $ do
        it "creates a counter machine from #MOC CM 2 with all valid ops and preds" $
            shouldHave (parseMoC "#MOC CM 2")
                ["R1-1", "R1+1", "R2-1", "R2+1"]
                ["R1=0", "R2=0"]

        it "creates a counter machine without ops and preds for non-existing registers" $
            shouldNotHave (parseMoC "#MOC cm 1")
                ["R0+1", "R2+1", "R1+0"]
                ["R0=0", "R2=0", "R1=1"]

        it "creates a stack machine from #MOC sm 2 \"Ab\" with valid ops and preds" $
            shouldHave (parseMoC "#MOC sm 2 \"Ab\"")
                ["R1+A", "R1+b", "R2+A", "R2+b"]
                ["R1=_", "R1=A", "R1=b", "R2=_", "R2=A", "R2=b"]

        it "creates a stack machine without ops and preds for non-existing registers" $
            shouldNotHave (parseMoC "#MOC SM 1 \"A\"")
                ["R0+A", "R1+B", "R2+A"]
                ["R0=A", "R1=B", "R2=A"]

        it "doesn't create a stack machine with a bad alphabet" $
            (evaluate $ parseMoC "#MOC SM 3 \"ABC%&å:ý\"") `shouldThrow` errorCall
                "Error parsing arguments for stack machine: Alphabet needs to be non-empty and can only consist of alphanumerical symbols"

        it "doesn't create a counter machine from definition with bogus arguments" $
            (evaluate $ parseMoC "#MOC cm 1 2 3 asdfae ääüülö") `shouldThrow` errorCall
                "Error parsing arguments for counter machine: Incorrect number of arguments"

    describe "addOperation" $ do
        let moc = parseMoC "#MOC CM 3"

        it "throws an error when given operation is already present" $
            (evaluate $ addOperation moc "R1+1" id) `shouldThrow` errorCall
                "Error adding an operation to model of computation: Operation R1+1 already exists"


shouldHave :: MoC -> [OpName] -> [PredName] -> Expectation
shouldHave moc opnames prednames =
    hasOps && hasPreds `shouldBe` True
    where
        hasOps   = and $ map (\o -> isJust $ ops moc o) opnames
        hasPreds = and $ map (\p -> isJust $ preds moc p) prednames


shouldNotHave :: MoC -> [OpName] -> [PredName] -> Expectation
shouldNotHave moc opnames prednames =
    notHasOps && notHasPreds `shouldBe` True
    where
        notHasOps   = and $ map (\o -> isNothing $ ops moc o) opnames
        notHasPreds = and $ map (\p -> isNothing $ preds moc p) prednames
