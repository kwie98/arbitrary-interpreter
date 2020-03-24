module ArbitraryInterpreter.Exec.PreExecCheckSpec
( spec
) where

import ArbitraryInterpreter.Exec.PreExecCheck
import ArbitraryInterpreter.Parse.ParseMoC
import ArbitraryInterpreter.Parse.ParseProgram
import Control.Exception (evaluate)
import System.IO
import System.Directory
import Test.Hspec

cpath = "C:\\Users\\Konrad\\Documents\\Uni\\arbitrary-interpreter\\test\\programs\\correct\\"
fpath = "C:\\Users\\Konrad\\Documents\\Uni\\arbitrary-interpreter\\test\\programs\\faulty\\"


spec :: Spec
spec = do
    correctPrograms <- runIO $ readFiles cpath
    faultyPrograms  <- runIO $ readFiles fpath

    describe "preExecCheck" $ do
        mapM_ (it "lets correct programs pass") $
            [preExecCheck (parseProgram x) (parseMoC x) `shouldBe` True | x <- correctPrograms]

    describe "preExecCheck" $ do
        mapM_ (\x -> it "doesn't let faulty programs pass" $ do x) $
            [(evaluate $ preExecCheck (parseProgram x) (parseMoC x)) `shouldThrow` anyErrorCall | x <- faultyPrograms]


-- return list of all programs in given dir
readFiles :: FilePath -> IO [String]
readFiles path = do
    paths <- fmap (fmap (path ++)) $ listDirectory path
    mapM (readFile) paths
