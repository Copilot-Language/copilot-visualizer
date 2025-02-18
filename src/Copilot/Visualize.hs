-- | Graphical visualization of Copilot specifications.

{-# LANGUAGE DeriveGeneric #-}

module Copilot.Visualize
  where

-- External imports
import Data.Aeson
import Data.List
import Data.Maybe             ( fromMaybe, isJust, isNothing )
import GHC.Generics
import System.Directory
import System.Directory.Extra
import System.FilePath
import Text.Printf
import Text.Read

-- External imports: Copilot
import Copilot.Core
import Copilot.Interpret.Eval

-- Internal imports
import Paths_copilot_visualizer

-- | Generate a visualization of a specification for a given number of steps.
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

-- | Generate an abstract representation of a trace of a specification
-- interpreted for a given number of steps.
makeTraceEval :: Int
              -> Spec
              -> ExecTrace
              -> AppData
makeTraceEval k spec e =
    AppData (observerTEs ++ triggerTEs) (k - 1)
  where
    observerTEs = map mkTraceElem (interpObserversOpt spec e)
    triggerTEs  = map mkTraceElem (interpTriggersWithArgs spec e)

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

-- * Abstract representation of a trace

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
    { tvValue   :: String
    , tvIsEmpty :: Bool
    }
  deriving (Generic, Show)

instance ToJSON TraceValue

-- * Auxiliary functions

-- | Compute the list of values associated to observers.
interpObserversOpt :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpObserversOpt _spec e =
  map (\(n, os) -> (n, map Just os)) (interpObservers e)

-- | Compute the list of values associated to triggers and their arguments.
--
-- For each trigger, we first include the values of the trigger itself, and
-- then the values of the arguments to the trigger.
interpTriggersWithArgs :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpTriggersWithArgs spec e = concatMap triggerOutputs (interpTriggers e)
  where
    -- This function adds one more output for the trigger itself.
    triggerOutputs :: (String, [Maybe [Output]]) -> [(String, [Maybe Output])]
    triggerOutputs (triggerName, triggerArgs) =
        (triggerName, triggerValues) : zip argNames argValues
      where
        triggerValues = map triggerValue triggerArgs

	-- Value for the trigger at a given time, based on the values of its
        -- arguments.
        triggerValue :: Maybe [Output] -> Maybe Output
        triggerValue Nothing  = Just "false"
        triggerValue (Just _) = Just "true"

        -- Names and values for the arguments.
        argNames  = map (\ix -> triggerName ++ "Arg" ++ show ix) [0..]
        argValues = transpose (transMaybes numArgs triggerArgs)
        numArgs   = triggerNumArgs spec triggerName

-- Number of arguments to a trigger in a spec.
--
-- PRE: name exists as a trigger in spec.
triggerNumArgs :: Spec -> String -> Int
triggerNumArgs spec name =
  case find (\t -> triggerName t == name) (specTriggers spec) of
    Nothing -> error "Couldn't find given trigger in spec, should never occur!"
    Just t  -> length $ triggerArgs t

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

-- | Show a floating point value.
showValueFloat :: Output -> String
showValueFloat = formatFloat . read
  where
    formatFloat :: Double -> String
    formatFloat = printf "%.2g"

-- | Given a list of maybe lists of known length, this function creates a list
-- of lists, pushing the Maybe's inside.
transMaybes :: Int -> [Maybe [a]] -> [[Maybe a]]
transMaybes = map . transMaybes'
  where
    transMaybes' argsLength (Just xs) = map Just xs
    transMaybes' argsLength Nothing   = replicate argsLength Nothing
