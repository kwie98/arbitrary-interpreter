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
    { validState :: MachineState -> Maybe String
    , ops        :: OpName       -> Maybe (MachineState -> MachineState)
    , preds      :: PredName     -> Maybe (MachineState -> Bool)
    }

-- extension for allowing permuting machine states for MoC's with registers and
-- for printing machine states in a csv-format
data MoCInfo = MoCInfo
    { registers     :: Int
    , permuteMState :: Maybe ([Int] -> MachineState -> MachineState)
    , printMState   :: Maybe (MachineState -> String)
    }

data ExtendedMoC = ExtendedMoC
    { getMoC  :: MoC
    , getInfo :: Maybe MoCInfo
    }

-- alphabet for DFAMOC, LBAMOC, RDPAMOC, TMMOC
validSymbols = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!%&()*+,-.<>?@[]^_{|}~"
-- smaller alphabet for SMMOC, ISMMOC
validSMSymbols = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
