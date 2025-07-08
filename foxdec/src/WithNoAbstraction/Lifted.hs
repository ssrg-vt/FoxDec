{-# LANGUAGE PartialTypeSignatures , FlexibleContexts, StrictData #-}
{-# OPTIONS_HADDOCK prune  #-}

{-|
Module      : Lifted  
Description : Types for a lifted IR
-}



module WithNoAbstraction.Lifted where

import Data.SValue
import Data.SPointer
import Data.L0
import WithAbstractSymbolicValues.Class
import Binary.Generic


-- Types for storing a lifted representation of a binary.

type Lifted = Lifting Binary (Sstate SValue SPointer) (FInit SValue SPointer) SValue
type LiftedWithEntry = LiftingEntry Binary (Sstate SValue SPointer) (FInit SValue SPointer) SValue



