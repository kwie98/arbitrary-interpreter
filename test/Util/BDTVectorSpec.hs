module Util.BDTVectorSpec (spec) where

import Test.Hspec
import Data.List
import qualified Data.Vector as Vector
import Util.BDTVector

spec :: Spec
spec = do
    let v1 = Vector.fromList ["root", "fc", ""]

    describe "leaves" $ do
        it "returns an empty list when given an empty BDT" $
            leaves Vector.empty `shouldBe` []

        it "returns the root of a singleton BDT" $
            (leaves $ Vector.singleton "ROOT") `shouldBe` ["ROOT"]

        it "returns both children of root from BDT of size 3" $
            (leaves $ Vector.generate 3 (show)) `shouldBe` ["1", "2"]

        it "returns only child of root from BDT of size 2" $
            (leaves $ Vector.generate 2 (show)) `shouldBe` ["1"]

        it ("returns only child of root from BDT " ++ show v1) $
            leaves v1 `shouldBe` ["fc"]

        it "finds all children of full BDT of size (n ^ 2 - 2)" $
            (leaves $ Vector.generate 30 (show)) `shouldBe` map (show) [15..29]

    describe "branches" $ do
        it "returns an empty list when given an empty BDT" $
            branches Vector.empty `shouldBe` []

        it "returns an empty list when given a singleton BDT" $
            (branches $ Vector.singleton "ROOT") `shouldBe` []

        it "returns root from BDT of size 2" $
            (branches $ Vector.generate 2 (show)) `shouldBe` ["0"]

        it "returns root from BDT of size 3" $
            (branches $ Vector.generate 2 (show)) `shouldBe` ["0"]

        it "returns root and first child of root from BDT of size 5" $
            (branches $ Vector.generate 5 (show)) `shouldBe` ["0", "1"]

        it ("returns only root from BDT " ++ show v1) $
            branches v1 `shouldBe` ["root"]

        it "finds all branches of full BDT of size (n ^ 2 - 2)" $
            (sort . branches $ Vector.generate 30 (show)) `shouldBe` (sort $ map (show) [0..14])
