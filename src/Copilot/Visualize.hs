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
import Debug.Trace

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
    , tvIsBoolean :: Bool
    , tvIsFloat   :: Bool
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
    -- print subs
    -- print f
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
    observerTEs = map observerTE obsvs

    triggerTEs  = concatMap triggerTE trigs

    observerTE :: (String, [Output]) -> TraceElem
    observerTE (name, outputs) = TraceElem
        { teName   = name
        , teValues = values
        , teIsBoolean = any tvIsBoolean values
        , teIsFloat   = any tvIsFloat values
        }
      where
        values = map teVal outputs
        teVal x = TraceValue (showValue x) (isBoolean x) (isFloat x) (x == "--")

    showValue s | isBoolean s = showValueBoolean s
                | isFloat s   = showValueFloat s
                | otherwise   = s

    showValueBoolean :: String -> String
    showValueBoolean "true"  = "true"
    showValueBoolean "false" = "false"

    showValueFloat :: String -> String
    showValueFloat "--" = "--"
    showValueFloat s = formatFloat $ read s
      where
        formatFloat :: Double -> String
        formatFloat = printf "%.2g"

    triggerTE :: (String, [[String]]) -> [TraceElem]
    triggerTE (name, ls) =
        map triggerArgTE (zip ls ("" : map (\x -> "Arg" ++ show x) [0..]))
      where
        triggerArgTE (values, i) =
            TraceElem { teName   = name ++ i
                      , teIsBoolean = any tvIsBoolean values'
                      , teIsFloat   = any tvIsFloat values'
                      , teValues = values'
                      }
          where
            values' = map teVal values
            teVal x = TraceValue (showValue x) (isBoolean x) (isFloat x) (x == "--")

    trigs :: [(String, [[String]])]
    trigs = map (printOutputs . regroup) trigs'
      where
        printOutputs :: (String, [[Maybe Output]]) -> (String, [[String]])
        printOutputs (nm, ls) = (nm, transpose $ map (map ppTriggerOutput) ls)

        ppTriggerOutput :: Maybe Output -> String
        ppTriggerOutput Nothing  = "--"
        ppTriggerOutput (Just v) = v

        regroup :: (String, [Maybe [Output]], Int) -> (String, [[Maybe Output]])
        regroup (n, ls, len) = (n, map rep ls)
          where
            rep :: Maybe [Output] -> [Maybe Output]
            rep Nothing  = Just "false" : replicate len Nothing
            rep (Just x) = Just "true"  : map Just x

    trigs' :: [(String, [Maybe [Output]], Int)]
    trigs' = map addArgsLength (interpTriggers e)
      where
        addArgsLength :: (String, [Maybe [Output]]) -> (String, [Maybe [Output]], Int)
        addArgsLength (name, output) = (name, output, argsLength)
          where
            argsLength = case find (\t -> triggerName t == name) (specTriggers spec) of
              Nothing -> error "Couldn't find given trigger in spec, should never occur!"
              Just t -> length $ triggerArgs t

    -- Push Maybe's to inner level.
    transMaybes :: [Maybe [Output]] -> Int -> [[Maybe Output]]
    transMaybes []       _          = []
    transMaybes (xs:xss) argsLength = case xs of
      Just xs' -> map Just xs' : transMaybes xss argsLength
      Nothing  -> replicate argsLength Nothing : transMaybes xss argsLength

    obsvs :: [(String, [Output])]
    obsvs = interpObservers e

paint :: Int     -- ^ Number of steps to interpret.
      -> Spec    -- ^ Specification to interpret.
      -> IO ()
paint k spec =
    paintEvaluation k spec e
  where
    e = eval Haskell k spec

paintEvaluation :: Int
                -> Spec
                -> ExecTrace
                -> IO ()
paintEvaluation k spec e = do
    putStrLn "\\documentclass{standalone}"
    putStrLn "\\usepackage{tikz}"
    putStrLn "\\usepackage{tikz-timing}"
    putStrLn "\\usepackage{xcolor}"
    putStrLn "\\definecolor{false}{HTML}{ECD9ED}"
    putStrLn "\\definecolor{true}{HTML}{D9ECED}"
    putStrLn "\\begin{document}"
    putStrLn "\\tikzset{"
    putStrLn "every picture/.style={"
    putStrLn "  execute at end picture={"
    putStrLn "    \\path (current bounding box.south west) +(-1,-1) (current bounding box.north east) +(1,1);"
    putStrLn "    }"
    putStrLn "}"
    putStrLn "}"
    putStrLn ""
    putStrLn "\\tikzset{"
    putStrLn "timing/.style={x=5ex,y=2ex},"
    putStrLn "timing/rowdist=4ex,"
    putStrLn "timing/dslope=0.1,"
    putStrLn "x=5ex,"
    putStrLn "timing/coldist=1ex,"
    putStrLn "timing/name/.style={font=\\sffamily\\scriptsize},"
    putStrLn "timing/d/text/.style={font=\\sffamily\\tiny},"
    putStrLn "}"
    putStrLn "\\begin{tikztimingtable}"
    printObserverOutputs
    printTriggerOutputs
    putStrLn "\\extracode"
    putStrLn "\\begin{background}[shift={(0.05,0)},help lines]"
    putStrLn $ "\\vertlines[help lines,opacity=0.3]{-0.3ex,...,"
                ++ show (k - 1)
                ++ "}"
    putStrLn "\\end{background}"
    putStrLn "\\end{tikztimingtable}"
    putStrLn "\\end{document}"
  where
    signal :: String -> [String] -> String
    signal name values = name <> " & g" <> concat values <> "\\\\"

    printTriggerOutputs :: IO ()
    printTriggerOutputs = mapM_ putStrLn (map printTriggerOutput trigs)
      where
        printTriggerOutput :: (String, [Maybe [Output]], Int) -> String
        printTriggerOutput (name, ls, argsLength) =
          signal name trig
          <>
          "\n"
          <>
          concatMap (\(v, n) -> signal n (showValues v)) (zip (args argsLength) argNames)
          where
            trig :: [String]
            trig = concatMap printTriggerOutputListElem ls

            args :: Int -> [[Maybe Output]]
            args argsLength = transpose $ transMaybes ls argsLength

            argNames = [name <> " arg \\#" <> show n | n <- [0..]]

        -- Push Maybe's to inner level.
        transMaybes :: [Maybe [Output]] -> Int -> [[Maybe Output]]
        transMaybes []       _          = []
        transMaybes (xs:xss) argsLength = case xs of
          Just xs' -> map Just xs' : transMaybes xss argsLength
          Nothing  -> replicate argsLength Nothing : transMaybes xss argsLength

        -- Ignores the value, just interprets as a boolean
        printTriggerOutputListElem :: Maybe [Output] -> [String]
        printTriggerOutputListElem Nothing  = ["[fill=false]D{F}"]
        printTriggerOutputListElem (Just _) = ["[fill=true]D{T}"]

    ---------------------------------------------------------------------------
    printObserverOutputs :: IO ()
    printObserverOutputs = mapM_ putStrLn (map observerOutput obsvs)
      where
        observerOutput (name, ls) = signal name (showValues $ map Just ls)

    ---------------------------------------------------------------------------
    showValues :: [Maybe String] -> [String]
    showValues = map showValue

    showValue :: Maybe String -> String
    showValue Nothing = "S"
    showValue (Just s) | isBoolean s = showValueBoolean s
                       | isFloat s   = showValueFloat s
                       | otherwise   = showValueNumber s

    showValueBoolean :: String -> String
    showValueBoolean "true"  = "[fill=true]D{T}"
    showValueBoolean "false" = "[fill=false]D{F}"

    showValueFloat :: String -> String
    showValueFloat = formatFloat . read
      where
        formatFloat :: Double -> String
        formatFloat = printf "D{%.2g}"

    showValueNumber :: String -> String
    showValueNumber n = "D{" <> n <> "}"
    ---------------------------------------------------------------------------

    trigs :: [(String, [Maybe [Output]], Int)]
    trigs = map addArgsLength (interpTriggers e)
      where
        addArgsLength :: (String, [Maybe [Output]]) -> (String, [Maybe [Output]], Int)
        addArgsLength (name, output) = (name, output, argsLength)
          where
            argsLength = case find (\t -> triggerName t == name) (specTriggers spec) of
              Nothing -> error "Couldn't find given trigger in spec, should never occur!"
              Just t -> length $ triggerArgs t

    obsvs :: [(String, [Output])]
    obsvs = interpObservers e

isBoolean "true" = True
isBoolean "false" = True
isBoolean _ = False

isFloat s =
  isJust asInt || isJust asFloat
  where
    asInt :: Maybe Int
    asInt = readMaybe s

    asFloat :: Maybe Float
    asFloat = readMaybe s
