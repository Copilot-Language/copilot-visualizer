-- | Graphical visualization of Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Visualize
    ( paint )
  where

import Data.Maybe (isNothing, isJust)
import Data.List
import Copilot.Core
import Copilot.Interpret.Eval
import Text.Printf
import Text.Read

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
  isJust asInt || isNothing asFloat
  where
    asInt :: Maybe Int
    asInt = readMaybe s

    asFloat :: Maybe Float
    asFloat = readMaybe s
