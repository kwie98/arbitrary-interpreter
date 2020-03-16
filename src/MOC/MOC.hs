module MoC.MoC where

import Data.Maybe

data MoC = MoC {
    validState :: (String -> Bool) ,
    ops :: (String -> (Maybe (String -> String))) ,
    preds :: (String -> (Maybe (String -> Bool)))
}


isOp :: MoC -> String -> Bool
isOp moc s
    | s == "NOP"         = True
    | isJust $ ops moc s = True
    | otherwise          = False


isPred :: MoC -> String -> Bool
isPred moc s = isJust $ preds moc s
