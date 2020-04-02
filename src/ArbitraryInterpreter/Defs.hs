module ArbitraryInterpreter.Defs where

import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (Maybe)

-- components of a program
type ProgramState = String
type OpName       = String
type PredName     = String
type BDTVector    = Vector String -- includes PredName and ProgramState

type Program      = HashMap ProgramState (OpName, BDTVector)
type ProgramName  = String

-- string representation of the internal state of an arbitrary model of computation (MoC)
type MachineState = String

data MoC = MoC
    { validState :: (MachineState -> Bool)
    , ops        :: (OpName       -> (Maybe (MachineState -> MachineState)))
    , preds      :: (PredName     -> (Maybe (MachineState -> Bool)))
    }
