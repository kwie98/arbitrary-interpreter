module ArbitraryInterpreter.Exec.RunProgramSpec
( spec
) where

import ArbitraryInterpreter.Defs
import ArbitraryInterpreter.Exec.RunProgram
import ArbitraryInterpreter.Parse.ParseCollection
import Data.List (permutations)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
    sub10 <- runIO $ readFile ("test" </> "programs" </> "greyzone" </> "sub10.mp")
    let sub10permuts = zip ([1..]) (map (parseCollection) $ permuteProgCollection sub10)
    -- runIO $ mapM_ (putStrLn) permuts

    sub <- runIO $ readFile ("test" </> "programs" </> "greyzone" </> "sub.mp")
    let subpermuts = zip ([1..]) (map (parseCollection) $ permuteProgCollection sub)

    subcalls <- runIO $ readFile ("test" </> "programs" </> "greyzone" </> "badsubcalls.mp")
    let subcallspermuts = zip ([1..]) (map (parseCollection) $ permuteProgCollection subcalls)

    describe "run" $ do
        mapM_ (uncurry xit) $
            [("executes permutation " ++ show i ++ " of sub10",
            (run Nothing moc (progMap Map.! "sub10") "[100]") `shouldBe` ("End", "[90]")) | (i, (moc, progMap)) <- sub10permuts]

        mapM_ (uncurry xit) $
            [("executes permutation " ++ show i ++ " of sub4",
            (run Nothing moc (progMap Map.! "sub4") "[100]") `shouldBe` ("End", "[96]")) | (i, (moc, progMap)) <- subpermuts]

        mapM_ (uncurry xit) $
            [("executes permutation " ++ show i ++ " of subcalls",
            (run Nothing moc (progMap Map.! "MAIN") "[3, 4, 5]") `shouldBe` ("End", "[0,0,8]")) | (i, (moc, progMap)) <- subcallspermuts]


-- given collection file, return permutations where order of programs is shuffled
permuteProgCollection :: String -> [String]
permuteProgCollection text = foo
    where
        trimmedText = trimProgramText text
        mocdef = head trimmedText
        foo = map (\perm -> unlines (mocdef : (lines perm)))
            (map (unlines) $ permutations . collapsePrograms' $ tail trimmedText)


-- similar to ParseCollection.collapsePrograms, but returns list of program texts
collapsePrograms' :: [String] -> [String]
collapsePrograms' [] = []
collapsePrograms' (fl1:flines) = case isProgramDef fl1 of
    True  -> (unlines $ (fl1 : takeWhile f flines)) : (collapsePrograms' $ dropWhile f flines)
    False -> error $ err ++ "Expected program definition, got: " ++ fl1
    where
        f = not . isProgramDef
        err = "Error doing program order permutation test: "
