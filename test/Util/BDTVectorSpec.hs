module Util.BDTVectorSpec (spec) where

import Test.Hspec
import Data.List
import Control.Monad
import qualified Data.Vector as Vector
import Util.BDTVector

spec :: Spec
spec = do
    let v1 = Vector.fromList ["root", "fc", ""]

    describe "leaves" $ do
        it "returns an empty list when given an empty BDT" $
            leaves Vector.empty `shouldMatchList` []

        it "returns the root of a singleton BDT" $
            (leaves $ Vector.singleton "ROOT") `shouldMatchList` ["ROOT"]

        it "returns both children of root from BDT of size 3" $
            (leaves $ Vector.generate 3 (show)) `shouldMatchList` ["1", "2"]

        it "returns only child of root from BDT of size 2" $
            (leaves $ Vector.generate 2 (show)) `shouldMatchList` ["1"]

        it ("returns only child of root from BDT " ++ show v1) $
            leaves v1 `shouldMatchList` ["fc"]

        it "finds all children of full BDT of size (n ^ 2 - 1)" $
            (leaves $ Vector.generate 31 (show)) `shouldMatchList` map (show) [15..30]

    describe "branches" $ do
        it "returns an empty list when given an empty BDT" $
            branches Vector.empty `shouldMatchList` []

        it "returns an empty list when given a singleton BDT" $
            (branches $ Vector.singleton "ROOT") `shouldMatchList` []

        it "returns root from BDT of size 2" $
            (branches $ Vector.generate 2 (show)) `shouldMatchList` ["0"]

        it "returns root from BDT of size 3" $
            (branches $ Vector.generate 2 (show)) `shouldMatchList` ["0"]

        it "returns root and first child of root from BDT of size 5" $
            (branches $ Vector.generate 5 (show)) `shouldMatchList` ["0", "1"]

        it ("returns only root from BDT " ++ show v1) $
            branches v1 `shouldMatchList` ["root"]

        it "finds all branches of full BDT of size (n ^ 2 - 1)" $
            (branches $ Vector.generate 31 (show)) `shouldMatchList`
            (map (show) [0 .. 14])


            -- TODO branches ++ leaves always returns list of all nodes of BDT
