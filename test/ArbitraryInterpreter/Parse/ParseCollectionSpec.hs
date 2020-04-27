module ArbitraryInterpreter.Parse.ParseCollectionSpec
( spec
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Parse.PreExecCheck
import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseCollection
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Control.Exception (evaluate)
import System.Directory
import System.FilePath ((</>))
import System.IO
import Test.Hspec

-- paths to testing programs
cpath = "test" </> "programs" </> "correct"
fpath = "test" </> "programs" </> "faulty"

spec :: Spec
spec = do
    correctPrograms <- runIO $ readFiles cpath
    faultyPrograms  <- runIO $ readFiles fpath
    let variations = concat $ map (makeVariations) correctPrograms

    describe "preExecChecks" $ do
        mapM_ (uncurry it) $
            [("lets correct program " ++ pname ++ " pass",
            (uncurry preExecChecks $ parseCollection p) `shouldBe` True) | (pname, p) <- correctPrograms]

    -- describe "preExecChecks" $ do
    --     mapM_ (\expectation -> (uncurry it) $ do expectation) $
    --         [("doesn't let faulty variation of correct program " ++ pname ++ " pass",
    --         (evaluate $ uncurry preExecChecks $ parseCollection p) `shouldThrow` anyErrorCall) | (pname, p) <- variations]

    describe "preExecChecks" $ do
        mapM_ (\expectation -> (uncurry it) $ do expectation) $
            [("doesn't let faulty program " ++ pname ++ " pass",
            (evaluate $ uncurry preExecChecks $ parseCollection p) `shouldThrow` anyErrorCall) | (pname, p) <- faultyPrograms]


-- same as preExecCheck, but for a collection of programs with a common MoC.
-- Does not check whether the MoC actually includes all programs as operations,
-- only those which are actually used.
preExecChecks :: ExtendedMoC -> Map.Map ProgramName Program -> Bool
preExecChecks xmoc programs = all (\p -> preExecCheck xmoc p) programs


shouldEvaluate :: IO a -> Expectation
shouldEvaluate action = (action `shouldThrow` anyErrorCall) `shouldReturn` ()


-- return list of all programs in given dir together with their file names
readFiles :: FilePath -> IO [(FilePath, String)]
readFiles path = do
    paths <- fmap (fmap (\p -> path </> p)) $ listDirectory path
    files <- mapM (readFile) paths
    return $ zip paths files


-- for a program and its path, return a list of programs which are the original
-- program but with one line removed
makeVariations :: (FilePath, String) -> [(FilePath, String)]
makeVariations (pname, p) =
    zip (repeat pname) [unlines $ deleteAt i plines | i <- [0 .. (length plines - 1)]]
    where
        plines = trimProgramText p


-- delete element at given index
deleteAt :: Int -> [a] -> [a]
deleteAt _ []     = []
deleteAt i (a:as)
    | i == 0    = as
    | otherwise = a : deleteAt (i-1) as
