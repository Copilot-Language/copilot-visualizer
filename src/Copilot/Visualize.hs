-- | Graphical visualization of Copilot specifications.

{-# LANGUAGE DeriveGeneric #-}

module Copilot.Visualize
  where

import Copilot.Core
import Copilot.Interpret.Eval
import Data.Aeson
import Data.List
import Data.Maybe               ( fromMaybe, isJust, isNothing )
import GHC.Generics
import Paths_copilot_visualizer
import System.Directory
import System.Directory.Extra
import System.FilePath
import Text.Printf
import Text.Read

data AppData = AppData
    { adTraceElems :: Trace
    , adLastSample :: Int
    }
  deriving (Generic, Show)

instance ToJSON AppData

type Trace = [ TraceElem ]

data TraceElem = TraceElem
    { teName      :: String
    , teIsBoolean :: Bool
    , teIsFloat   :: Bool
    , teValues    :: [ TraceValue ]
    }
  deriving (Generic, Show)

instance ToJSON TraceElem

data TraceValue = TraceValue
    { tvValue     :: String
    , tvIsEmpty   :: Bool
    }
  deriving (Generic, Show)

instance ToJSON TraceValue

makeTrace :: Int     -- ^ Number of steps to interpret.
          -> Spec    -- ^ Specification to interpret.
          -> IO ()
makeTrace k spec = do
    dir <- getDataDir
    let f = dir </> "data"
    let subs = toJSON $ makeTraceEval k spec e
    copyTemplate f subs "target"
  where
    e = eval Haskell k spec

makeTraceEval :: Int
              -> Spec
              -> ExecTrace
              -> AppData
makeTraceEval k spec e =
    AppData (observerTEs ++ triggerTEs) (k - 1)
  where
    observerTEs = map mkTraceElem (interpObserversOpt spec e)
    triggerTEs  = map mkTraceElem (interpTriggersWithArgs spec e)

interpObserversOpt :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpObserversOpt spec e =
  map (\(n, os) -> (n, map Just os)) (interpObservers e)

-- Compute the list of values associated to a trigger and its arguments, which
-- the first values being the ones for the trigger itself, and subsequent
-- values being those of the arguments to the trigger.
interpTriggersWithArgs :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpTriggersWithArgs spec e = concatMap triggerOutputs (interpTriggers e)
  where
    -- This function adds one more output for the trigger itself.
    triggerOutputs :: (String, [Maybe [Output]]) -> [(String, [Maybe Output])]
    triggerOutputs (triggerName, triggerArgs) =
        (triggerName, triggerValues) : zip argNames argValues
      where
        triggerValues = map triggerValue triggerArgs

        triggerValue :: Maybe a -> Maybe Output
        triggerValue Nothing  = Just "false"
        triggerValue (Just _) = Just "true"

        argNames  = map (\ix -> triggerName ++ "Arg" ++ show ix) [0..]
        argValues = transpose (transMaybes triggerArgs numArgs)
        numArgs   = triggerNumArgs spec triggerName

mkTraceElem :: (String, [Maybe Output]) -> TraceElem
mkTraceElem (name, outputs) = TraceElem
    { teName      = name
    , teValues    = values
    , teIsBoolean = any (isBoolean . tvValue) values
    , teIsFloat   = any (isFloat . tvValue) values
    }
  where
    values = map mkTraceValue outputs

mkTraceValue :: Maybe Output -> TraceValue
mkTraceValue x = TraceValue (showValue x) (isNothing x)

-- | True if the input value denotes a boolean value.
isBoolean :: String -> Bool
isBoolean "true"  = True
isBoolean "false" = True
isBoolean _       = False

-- | True if the input value denotes a floating point value.
isFloat :: String -> Bool
isFloat s =
    isJust asInt || isJust asFloat
  where
    asInt :: Maybe Int
    asInt = readMaybe s

    asFloat :: Maybe Float
    asFloat = readMaybe s

-- | Show a value.
showValue :: Maybe Output -> String
showValue Nothing  = "--"
showValue (Just s) | isFloat s = showValueFloat s
                   | otherwise = s

showValueFloat :: Output -> String
showValueFloat = formatFloat . read
  where
    formatFloat :: Double -> String
    formatFloat = printf "%.2g"

-- * Auxiliary functions

-- Number of arguments to a trigger in a spec.
--
-- PRE: name exists as a trigger in spec.
triggerNumArgs :: Spec -> String -> Int
triggerNumArgs spec name =
  case find (\t -> triggerName t == name) (specTriggers spec) of
    Nothing -> error "Couldn't find given trigger in spec, should never occur!"
    Just t  -> length $ triggerArgs t

-- | Given a list of maybe lists of known length, this function creates a list
-- of lists, pushing the Maybe's inside.
transMaybes :: [Maybe [a]] -> Int -> [[Maybe a]]
transMaybes []       _          = []
transMaybes (xs:xss) argsLength = case xs of
  Just xs' -> map Just xs' : transMaybes xss argsLength
  Nothing  -> replicate argsLength Nothing : transMaybes xss argsLength
