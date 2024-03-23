module Shared.Conditional.Loc (locEnabled) where

import Data.Bool (Bool (False))
import Data.Function (($))
import System.Environment.MrEnv (envAsBool)
import System.IO.Unsafe (unsafePerformIO)

locEnabled :: Bool
{-# NOINLINE locEnabled #-}
locEnabled = unsafePerformIO $ envAsBool "A_LOC" False
