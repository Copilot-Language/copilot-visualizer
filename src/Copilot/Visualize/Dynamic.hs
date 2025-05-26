{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Allow loading Copilot specifications from strings and modifying them live.
--
-- An example of a spec that can be passed as argument to the live simulator
-- follows:
--
-- @
--   spec :: String
--   spec = unlines
--     [ "let temperature :: Stream Word8"
--     , "    temperature = extern \"temperature\" (Just [0, 15, 20, 25, 30])"
--     , ""
--     , "    ctemp :: Stream Float"
--     , "    ctemp = (unsafeCast temperature) * (150.0 / 255.0) - 50.0"
--     , ""
--     , "    trueFalse :: Stream Bool"
--     , "    trueFalse = [True] ++ not trueFalse"
--     , ""
--     , "in do trigger \"heaton\"  (temperature < 18) [arg ctemp, arg (constI16 1), arg trueFalse]"
--     , "      trigger \"heatoff\" (temperature > 21) [arg (constI16 1), arg ctemp]"
--     , "      observer \"temperature\" temperature"
--     , "      observer \"temperature2\" (temperature + 1)"
--     ]
-- @
--
-- The imports are predefined.
module Copilot.Visualize.Dynamic where

-- External imports
import qualified Copilot.Core                 as Core
import           Copilot.Language             hiding (interpret, typeOf)
import           Language.Copilot             hiding (interpret, typeOf)
import qualified Language.Haskell.Interpreter as HI
import           Prelude                      hiding (div, not, (++), (<), (>))
import qualified Prelude

-- Internal imports
import Copilot.Visualize.Trace

data SimData = SimData
  { simSteps  :: Int
  , simSpec   :: Core.Spec
  , simString :: String
  }

simInit :: SimulationSettings -> String -> IO SimData
simInit simulationSettings spec = do
  -- Load the spec, and save it so that it doesn't need to be reloaded if it
  -- doesn't change.
  let numSteps = simulationSettingsInitialSteps simulationSettings
  spec' <- readSpec simulationSettings spec
  let simData = SimData numSteps spec' spec
  return simData

simStep :: SimulationSettings
        -> SimData
        -> String
        -> Maybe (Command, String)
        -> IO SimData
simStep simulationSettings simData msg pair = do
  let SimData numSteps spec' specS = simData

  -- Update the number of steps based on the input command received.
  let numSteps' = case msg of
                    "StepUp"   -> numSteps + 1
                    "StepDown" -> numSteps - 1
                    _          -> numSteps

  -- Update the spec based on the input command received.
  (spec'', specS') <- case (msg, pair) of

     ("StepUp",   _) -> pure (spec', specS)

     ("StepDown", _) -> pure (spec', specS)

     (_, Just (AddStream name expr, _)) -> do

       let specN = specS Prelude.++ "\n" Prelude.++
                   "      " Prelude.++ completeExpr
           completeExpr = concat [ "observer "
                                 , show name
                                 , " ("
                                 , expr
                                 , ")"
                                 ]
       let trace = extractTrace spec'
       spec2 <- readSpec simulationSettings specN
       let spec3 = updateWithTrace trace spec2
       return (spec3, specN)

     (_, Just (command, name)) ->
       (,) <$> apply simulationSettings spec' name command <*> pure specS

     _ -> pure (spec', specS)

  return (SimData numSteps' spec'' specS')

data SimulationSettings = SimulationSettings
  { simulationSettingsInitialSteps :: Int
  , simulationSettingsImports      :: [(String, Maybe String)]
  }


mkDefaultSimulationSettings :: SimulationSettings
mkDefaultSimulationSettings =
  SimulationSettings
    3
    [ ("Control.Monad.Writer",  Nothing)
    , ("Copilot.Language",      Nothing)
    , ("Copilot.Language.Spec", Nothing)
    , ("Data.Functor.Identity", Nothing)
    , ("Language.Copilot",      Nothing)
    , ("Prelude",               Just "P")
    ]

-- * Commands

-- * Load specs from strings

-- | Read a specification from a string and reify it.
readSpec :: SimulationSettings -> String -> IO Core.Spec
readSpec settings spec = do
  r <- HI.runInterpreter (loadSpec settings spec)
  case r of
    Left err -> do putStrLn $ "Error: " Prelude.++ show err
                   error $ show err
    Right s  -> return s

-- | Load a spec from a string, adding standard imports.
loadSpec :: SimulationSettings -> String -> HI.Interpreter Core.Spec
loadSpec settings spec = do
  HI.setImportsQ (simulationSettingsImports settings)
  spec' <- HI.interpret spec (HI.as :: Spec)
  HI.liftIO $ reify spec'

-- | Possible commands to affect a visualization.
data Command = Up Int
             | Down Int
             | AddStream String String
             | Noop
  deriving (Eq, Read, Show)

-- | Apply a command to a visualization.
apply :: SimulationSettings -> Core.Spec -> String -> Command -> IO Core.Spec
apply settings spec _name (AddStream sName sExpr) = do
  spec' <- addStream settings sName sExpr
  -- TODO: I need to bring the streams from the other spec too, otherwise the
  -- streams to include may refer to streams by ID that are in a different
  -- scope.
  let observers' = Core.specObservers spec'
      observers  = Core.specObservers spec
  return $ spec { Core.specObservers = observers Prelude.++ observers' }

apply _settings spec name command = pure $ spec
  { Core.specStreams =
      map (updateStream name command) (Core.specStreams spec)
  , Core.specObservers =
      map (updateObserver name command) (Core.specObservers spec)
  , Core.specTriggers =
      map (updateTrigger name command) (Core.specTriggers spec)
  }

-- | Apply a command to an observer.
updateObserver :: String -> Command -> Core.Observer -> Core.Observer
updateObserver name command (Core.Observer i e ty) =
  Core.Observer i (updateExpr name command e) ty

-- | Apply a command to a trigger.
updateTrigger :: String -> Command -> Core.Trigger -> Core.Trigger
updateTrigger name command (Core.Trigger i e es) = Core.Trigger i e' es'
  where
    e'  = updateExpr name command e
    es' = map (updateUExpr name command) es

-- | Apply a command to a core expression.
updateExpr :: String -> Command -> Core.Expr a -> Core.Expr a
updateExpr name command expr = case expr of
  (Core.ExternVar ty nameE vs)
    | nameE Prelude.== name
    -> Core.ExternVar ty nameE (updateValues vs ty command)

    | otherwise
    -> expr

  (Core.Op1 op expr1) ->
     Core.Op1 op (updateExpr name command expr1)

  (Core.Op2 op expr1 expr2) ->
     Core.Op2 op (updateExpr name command expr1) (updateExpr name command expr2)

  (Core.Op3 op expr1 expr2 expr3) ->
     Core.Op3
       op
       (updateExpr name command expr1)
       (updateExpr name command expr2)
       (updateExpr name command expr3)

  _ -> expr

-- | Apply a command to an untyped expression that carries the information
-- about the type of the expression as a value (existential).
updateUExpr :: String -> Command -> Core.UExpr -> Core.UExpr
updateUExpr name cmd (Core.UExpr ty expr) =
  Core.UExpr ty (updateExpr name cmd expr)

-- | Apply a command to a stream.
updateStream :: String -> Command -> Core.Stream -> Core.Stream
updateStream name command (Core.Stream i buf expr ty) =
  Core.Stream i buf (updateExpr name command expr) ty

-- | Apply a command to a series of typed values, if any.
updateValues :: Maybe [a] -> Type a -> Command -> Maybe [a]
updateValues vsM ty command =
  fmap (fmap (updateValue command ty) . zip [0..]) vsM

-- | Apply a command to a typed value.
updateValue :: Command -> Type a -> (Int, a) -> a
updateValue (Up n)   Core.Bool  (ix, a) =
  if n Prelude.== ix then Prelude.not a else a
updateValue (Down n) Core.Bool  (ix, a) =
  if n Prelude.== ix then Prelude.not a else a
updateValue (Up n)   Core.Word8 (ix, a) =
  if n Prelude.== ix then a + 1 else a
updateValue (Down n) Core.Word8 (ix, a) =
  if n Prelude.== ix then a - 1 else a
updateValue _        _          (_,  a) = a

-- * Sample spec

-- | Produce a specification from the expression for a stream.
addStream :: SimulationSettings -> String -> String -> IO Core.Spec
addStream settings name expr = do
  r <- HI.runInterpreter (addStream' settings name expr)
  case r of
    Left err   -> do putStrLn $ "Error: " Prelude.++ show err
                     error $ show err
    Right spec -> return spec

-- | Produce a specification from the expression for a stream, in an
-- interpreter context.
--
-- Observe that Interpreter () is an alias for InterpreterT IO ()
addStream' :: SimulationSettings -> String -> String -> HI.Interpreter Core.Spec
addStream' settings name expr = do
  HI.setImportsQ (simulationSettingsImports settings)
  let completeExpr = concat [ "observer ", show name, " (", expr, ")" ]

  spec <- HI.interpret completeExpr (HI.as :: Spec)
  HI.liftIO $ reify spec
