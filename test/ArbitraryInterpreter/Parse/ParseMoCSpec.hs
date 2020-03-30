module ArbitraryInterpreter.Parse.ParseMoCSpec
( spec
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.ParseMoC
import Data.Maybe (isJust)
import Test.Hspec


spec :: Spec
spec = do
    describe "parseMoC" $ do
        it "creates a counter machine from #cm 2 with valid ops" $
            parseMoC "#cm 2" `shouldHaveOps` ["R1-1", "R1+1", "R2-1", "R2+1"]


shouldHaveOps :: MoC -> [OpName] -> Expectation
shouldHaveOps moc opnames = foldl1 (&&) truths `shouldBe` True
    where
        truths = map (\opname -> isJust $ ops moc opname) opnames
