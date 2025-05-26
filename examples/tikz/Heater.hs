{-# LANGUAGE DeriveGeneric #-}
-- Copyright 2019 National Institute of Aerospace / Galois, Inc.

-- This is a simple example with basic usage. It implements a simple home
-- heating system: It heats when temp gets too low, and stops when it is high
-- enough. It read temperature as a byte (range -50C to 100C) and translates
-- this to Celsius.

module Main where

-- External imports
import Data.Aeson       (FromJSON)
import GHC.Generics     (Generic)
import Language.Copilot (Spec, Stream, Word8, arg, constI16, extern, not, reify,
                         trigger, unsafeCast, (++), (<), (>))
import Prelude          hiding (div, not, (++), (<), (>))

-- Internal imports
import Copilot.Visualize.Static

-- Reify the spec and produce a LaTeX file in the current directory with a Tikz
-- drawing of the spec.
main :: IO ()
main = reify (spec defaultInputs) >>= \spec' -> do
  makeTrace 3 spec' "tikz" "."

data Inputs = Inputs
  { tempValues :: Maybe [Word8]
  }
  deriving (Generic)

instance FromJSON Inputs

defaultInputs :: Inputs
defaultInputs = Inputs
  { tempValues = Just [ 10, 15, 18, 19, 25, 5, 45 ] }

spec :: Inputs -> Spec
spec inputs = do
    -- Triggers that fire when the ctemp is too low or too high,
    -- pass the current ctemp as an argument.
    trigger "heaton"  (ctemp < 18.0) [arg ctemp, arg (constI16 1), arg trueFalse]
    trigger "heatoff" (ctemp > 21.0) [arg (constI16 1), arg ctemp]
  where
    -- External temperature as a byte, range of -50C to 100C
    temp :: Stream Word8
    temp = extern "temperature" (tempValues inputs)

    -- Calculate temperature in Celsius.
    -- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as
    -- there is no direct relation between Word8 and Float.
    ctemp :: Stream Float
    ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

    trueFalse :: Stream Bool
    trueFalse = [True] ++ not trueFalse
