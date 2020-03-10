module MOC.MOC where

data MoC = MoC {
    validState :: (String -> Bool) ,
    ops :: (String -> (Maybe (String -> String))) ,
    preds :: (String -> (Maybe (String -> Bool)))
}
