module MoC.MoC where

import Data.Maybe

data MoC = MoC {
    validState :: (OpName -> Bool) ,
    ops :: (OpName -> (Maybe (MachineState -> MachineState))) ,
    preds :: (OpName -> (Maybe (MachineState -> Bool)))
}
type MachineState = String
type OpName       = String
type PredName     = String


isOp :: MoC -> OpName -> Bool
isOp moc s = isJust $ ops moc s



isPred :: MoC -> PredName -> Bool
isPred moc s = isJust $ preds moc s
