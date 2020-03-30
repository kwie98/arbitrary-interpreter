module ArbitraryInterpreter.Exec.PreExecCheckSpec
( spec
) where

import ArbitraryInterpreter.Exec.PreExecCheck
import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseProgram
import Control.Exception (evaluate)
import System.Directory
import System.FilePath ((</>))
import System.IO
import Test.Hspec

cpath = "test" </> "programs" </> "correct"
fpath = "test" </> "programs" </> "faulty"



spec :: Spec
spec = do
    correctPrograms <- runIO $ readFiles cpath
    faultyPrograms  <- runIO $ readFiles fpath
    let programVars = makeVariations $ head correctPrograms

    describe "preExecCheck" $ do
        mapM_ (it "lets correct programs pass") $
            [preExecCheck (parseProgram x) (parseMoC x) `shouldBe` True | x <- correctPrograms]

    describe "preExecCheck" $ do
        mapM_ (\x -> it "doesn't let faulty variations of correct programs pass" $ do x) $
            [(evaluate $ preExecCheck (parseProgram x) (parseMoC x)) `shouldThrow` anyErrorCall | x <- programVars]

    describe "preExecCheck" $ do
        mapM_ (\x -> it "doesn't let faulty programs pass" $ do x) $
            [(evaluate $ preExecCheck (parseProgram x) (parseMoC x)) `shouldThrow` anyErrorCall | x <- faultyPrograms]


-- return list of all programs in given dir
readFiles :: FilePath -> IO [String]
readFiles path = do
    paths <- fmap (fmap (\p -> path </> p)) $ listDirectory path
    mapM (readFile) paths


-- for a program, return a list of programs which are the original program but
-- with one line removed
makeVariations :: String -> [String]
makeVariations s = [unlines $ deleteAt i plines | i <- [0 .. (length plines - 1)]]
    where
        plines = filter (not . null) $ lines s


-- delete element at given index
deleteAt :: Int -> [a] -> [a]
deleteAt _ []     = []
deleteAt i (a:as)
    | i == 0    = as
    | otherwise = a : deleteAt (i-1) as
