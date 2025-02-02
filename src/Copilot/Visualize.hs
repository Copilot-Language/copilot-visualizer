{-# LANGUAGE DeriveGeneric #-}
-- | Graphical visualization of Copilot specifications.
--
-- This module contains an abstract representation of a trace as a series
-- named streams with their values, together with functions needed to
-- convert a 'Spec' into a trace.
--
-- The helper function 'makeTrace' expands a latex file that produces the
-- trace visualization as a figure.
module Copilot.Visualize
    ( AppData(..)
    , TraceElem(..)
    , TraceValue(..)

    , makeTrace
    , makeTraceEval
    )
  where

-- External imports
import Data.Aeson             (ToJSON (..))
import Data.Bifunctor         (second)
import Data.List              (find, transpose)
import Data.Maybe             (fromMaybe, isJust, isNothing)
import GHC.Generics           (Generic)
import System.Directory.Extra (copyTemplate)
import System.FilePath        ((</>))
import Text.Printf            (printf)
import Text.Read              (readMaybe)

-- External imports: Copilot
import Copilot.Core           (Spec (..), triggerArgs, triggerName)
import Copilot.Interpret.Eval (ExecTrace, Output, ShowType (Haskell), eval,
                               interpObservers, interpTriggers)

-- Internal imports
import Paths_copilot_visualizer (getDataDir)

-- * Abstract representation of a trace

-- | Data related to a specification.
data AppData = AppData
    { adTraceElems :: Trace
    , adLastSample :: Int
    }
  deriving (Generic, Show)

instance ToJSON AppData

-- | A trace, defined by a list of trace elements, each defining a stream (in
-- an abstract sense).
type Trace = [ TraceElem ]

-- | An element of a trace, or stream, with a name, its values, and information
-- about its type, needed to render it correctly.
data TraceElem = TraceElem
    { teName      :: String
    , teIsBoolean :: Bool
    , teIsFloat   :: Bool
    , teValues    :: [ TraceValue ]
    }
  deriving (Generic, Show)

instance ToJSON TraceElem

-- | An individual sample, with a value and an indication of whether the sample
-- exists.
data TraceValue = TraceValue
    { tvValue   :: String
    , tvIsEmpty :: Bool
    }
  deriving (Generic, Show)

instance ToJSON TraceValue

-- | Generate a visualization of a specification for a given number of steps.
makeTrace :: Int   -- ^ Number of steps to interpret.
          -> Spec  -- ^ Specification to interpret.
          -> IO ()
makeTrace k spec = do
  dir <- getDataDir
  let f = dir </> "data"
  let subs = toJSON $ makeTraceEval k spec $ eval Haskell k spec
  copyTemplate f subs "target"

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

-- | Generate an element of a trace from a stream name and its values over
-- time.
mkTraceElem :: (String, [Maybe Output]) -> TraceElem
mkTraceElem (name, outputs) = TraceElem
    { teName      = name
    , teValues    = values
    , teIsBoolean = any (isBoolean . tvValue) values
    , teIsFloat   = any (isFloat . tvValue) values
    }
  where
    values = map mkTraceValue outputs

-- | Generate an individual value (sample) of a trace.
mkTraceValue :: Maybe Output -> TraceValue
mkTraceValue x = TraceValue (showValue x) (isNothing x)

-- * Auxiliary functions

-- | Compute the list of values associated to observers.
interpObserversOpt :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpObserversOpt _spec = map (second (map Just)) . interpObservers

-- | Compute the list of values associated to triggers and their arguments.
--
-- For each trigger, we first include the values of the trigger itself, and
-- then the values of the arguments to the trigger.
interpTriggersWithArgs :: Spec -> ExecTrace -> [(String, [Maybe Output])]
interpTriggersWithArgs spec = concatMap triggerOutputs . interpTriggers
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
isFloat s = isJust asInt || isJust asFloat
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
