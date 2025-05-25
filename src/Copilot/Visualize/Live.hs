{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Run the interactive Copilot visualizer on a websocket.
--
-- This visualizer allows you to add new streams to the visualization. In
-- order to do so, it's necessary for the visualizer to have access to the
-- original spec, and to interpret new expressions in the same context as the
-- prior expressions.
--
-- An example of a spec that can be passed as argument to the visualizer
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
module Copilot.Visualize.Live
    ( visualize
    , visualizeWith
    , VisualSettings(..)
    , mkDefaultVisualSettings
    )
  where

import           Control.Concurrent.MVar             (newMVar, putMVar,
                                                      takeMVar, MVar)
import           Control.Exception                   (SomeException (..),
                                                      finally, handle)
import           Control.Monad                       (forever)
import qualified Copilot.Core                        as Core
import           Copilot.Interpret.Eval              (ShowType (Haskell), eval)
import           Copilot.Language                    hiding (interpret, typeOf)
import qualified Copilot.Language
import qualified Copilot.Visualize.Tikz              as View
import           Data.Aeson                          (ToJSON (..), encode)
import           Data.List                           hiding ((++))
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text                           as T
import qualified Data.Type.Equality                  as DE
import           Data.Typeable                       (Typeable)
import           GHC.Generics                        (Generic)
import           Language.Copilot                    hiding (interpret, typeOf)
import qualified Language.Haskell.Interpreter        as HI
import qualified Language.Haskell.Interpreter.Unsafe as HI
import qualified Network.WebSockets                  as WS
import           Prelude                             hiding (div, not, (++),
                                                      (<), (>))
import qualified Prelude
import           Text.Read                           (readMaybe)

-- | Open a websocket to listen to commands from the web visualization and
-- communicate results.
visualize :: String -> IO ()
visualize = visualizeWith mkDefaultVisualSettings

-- | Open a websocket to listen to commands from the web visualization and
-- communicate results.
visualizeWith :: VisualSettings -> String -> IO ()
visualizeWith settings spec = do
  putStrLn $ concat
    [ "WebSocket server starting on port "
    , show (visualSettingsPort settings)
    ,  "..."
    ]
  WS.runServer
    (visualSettingsHost settings)
    (visualSettingsPort settings)
    (app settings spec)

-- Settings used to customize the code generated.
data VisualSettings = VisualSettings
  { visualSettingsInitialSteps :: Int
                                  -- ^ Number of simulation steps to run
                                  -- initially.

  , visualSettingsHost         :: String
                                  -- ^ Host interface to listen to. Use
                                  -- "127.0.0.1" to listen at localhost.

  , visualSettingsPort         :: Int
                                  -- ^ Port to listen to.

  , visualSettingsImports      :: [(String, Maybe String)]
                                   -- ^ Imports, qualified of applicable.
  }

-- | Default settings that simulates 3 steps and listens on localhost at port
-- 9160.
mkDefaultVisualSettings :: VisualSettings
mkDefaultVisualSettings = VisualSettings
  { visualSettingsInitialSteps = 3
  , visualSettingsHost         = "127.0.0.1"
  , visualSettingsPort         = 9160
  , visualSettingsImports      = [ ("Control.Monad.Writer",  Nothing)
                                 , ("Copilot.Language",      Nothing)
                                 , ("Copilot.Language.Spec", Nothing)
                                 , ("Data.Functor.Identity", Nothing)
                                 , ("Language.Copilot",      Nothing)
                                 , ("Prelude",               Just "P")
                                 ]
  }

-- * Server

-- | Server application using web sockets.
app :: VisualSettings -> String -> WS.ServerApp
app settings spec pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ appInit settings spec conn

-- | Initialize the backend.
appInit :: VisualSettings -> String -> WS.Connection -> IO ()
appInit settings spec conn = handle appException $ do
    -- Save the literal (string) spec, so that it can be modified later.
    specV <- newMVar spec

    -- Load the spec, and save it so that it doesn't need to be reloaded if it
    -- doesn't change.
    spec' <- readSpec settings spec
    let numSteps = visualSettingsInitialSteps settings
    v <- newMVar (numSteps, spec')

    -- Obtain the static values of the trace.
    let appData = makeTraceEval' numSteps spec'

    -- Communicate the current values of the trace to the user interface.
    let samples = encode $ toJSON $ allSamples appData
    WS.sendTextData conn samples

    -- Start the application loop.
    appMainLoop settings conn v specV

  where

    appException :: SomeException -> IO ()
    appException e = do
      putStrLn $ "Error:" Prelude.++ show e
      error $ show e

appMainLoop :: VisualSettings
            -> WS.Connection
            -> MVar (Int, Core.Spec)
            -> MVar String
            -> IO ()
appMainLoop settings conn v specV = forever $ do
  msg <- T.unpack <$> WS.receiveData conn

  let pair :: Maybe (Command, String)
      pair = readMaybe msg

  (numSteps, spec') <- takeMVar v

  -- Update the number of steps based on the input command received.
  let numSteps' = case msg of
                    "StepUp"   -> numSteps + 1
                    "StepDown" -> numSteps - 1
                    _          -> numSteps

  -- Update the spec based on the input command received.
  spec'' <- case (msg, pair) of

              ("StepUp",   _) -> pure spec'

              ("StepDown", _) -> pure spec'

              (_, Just (AddStream name expr, _)) -> do
                specS <- takeMVar specV
                let specN = specS Prelude.++ "\n" Prelude.++
                            "      " Prelude.++ completeExpr
                    completeExpr = concat [ "observer "
                                          , show name
                                          , " ("
                                          , expr
                                          , ")"
                                          ]
                let trace = extractTrace spec'
                spec2 <- readSpec settings specN
                putMVar specV specN
                let spec3 = updateWithTrace trace spec2
                return spec3

              (_, Just (command, name)) -> apply settings spec' name command

              _ -> pure spec'

  -- Update the mvar with the new number of steps and spec.
  putMVar v (numSteps', spec'')

  let a       = makeTraceEval' numSteps' spec''
      samples = encode $ toJSON $ allSamples a
  WS.sendTextData conn samples

makeTraceEval' :: Int -> Core.Spec -> View.AppData
makeTraceEval' numSteps spec' =
  View.makeTraceEval numSteps spec' (eval Haskell numSteps spec')

-- | Data to communicate to the client.
data Data = Data
    { adLastSample :: Int
    , adTraceElems :: [TraceElem]
    }
  deriving (Generic)

instance ToJSON Data

-- | Series of samples associated to a stream.
data TraceElem = TraceElem
    { teName      :: String
    , teIsBoolean :: Bool
    , teValues    :: [Sample]
    }
  deriving (Generic)

instance ToJSON TraceElem

-- | Sample containing a time, value and duration.
data Sample = Sample
    { time     :: Int
    , value    :: String
    , duration :: Float
    }
  deriving (Generic)

instance ToJSON Sample

-- | Obtain all samples from a visualization.
allSamples :: View.AppData -> Data
allSamples appData = Data
  { adLastSample = View.adLastSample appData
  , adTraceElems = map toTraceElem (View.adTraceElems appData)
  }

-- | Convert a view trace element into a visualization trace element.
toTraceElem :: View.TraceElem -> TraceElem
toTraceElem te = TraceElem
  { teName      = View.teName te
  , teIsBoolean = View.teIsBoolean te
  , teValues    = zipWith
                    (\i v -> Sample i (View.tvValue v) 1)
                    [0..]
                    (View.teValues te)
  }

-- * Commands

-- | Possible commands to affect a visualization.
data Command = Up Int
             | Down Int
             | AddStream String String
             | Noop
  deriving (Eq, Read, Show)

-- | Apply a command to a visualization.
apply :: VisualSettings -> Core.Spec -> String -> Command -> IO Core.Spec
apply settings spec name (AddStream sName sExpr) = do
  spec' <- addStream settings sName sExpr
  -- TODO: I need to bring the streams from the other spec too, otherwise the
  -- streams to include may refer to streams by ID that are in a different
  -- scope.
  let observers' = Core.specObservers spec'
      observers  = Core.specObservers spec
  return $ spec { Core.specObservers = observers Prelude.++ observers' }

apply settings spec name command = pure $ spec
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
updateExpr name command e = case e of
  (Core.ExternVar ty nameE vs)
    | nameE Prelude.== name
    -> Core.ExternVar ty nameE (updateValues vs ty command)
    | otherwise
    -> e
  (Core.Op1 op e) ->
     Core.Op1 op (updateExpr name command e)
  (Core.Op2 op e1 e2) ->
     Core.Op2 op (updateExpr name command e1) (updateExpr name command e2)
  (Core.Op3 op e1 e2 e3) ->
     Core.Op3
       op
       (updateExpr name command e1)
       (updateExpr name command e2)
       (updateExpr name command e3)
  _ -> e

-- | Apply a command to an untyped expression that carries the information
-- about the type of the expression as a value (existential).
updateUExpr :: String -> Command -> Core.UExpr -> Core.UExpr
updateUExpr name cmd (Core.UExpr ty e) = Core.UExpr ty (updateExpr name cmd e)

-- | Apply a command to a stream.
updateStream :: String -> Command -> Core.Stream -> Core.Stream
updateStream name command (Core.Stream i b e ty) =
  Core.Stream i b (updateExpr name command e) ty

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
updateValue _        _          (ix, a) = a

-- * Sample spec

-- | Load a spec from a string, adding standard imports.
loadSpec :: VisualSettings -> String -> HI.Interpreter Core.Spec
loadSpec settings spec = do
  HI.setImportsQ (visualSettingsImports settings)
  spec' <- HI.interpret spec (HI.as :: Spec)
  HI.liftIO $ reify spec'

-- | Read a specification from a string and reify it.
readSpec :: VisualSettings -> String -> IO Core.Spec
readSpec settings spec = do
  r <- HI.runInterpreter (loadSpec settings spec)
  case r of
    Left err -> do putStrLn $ "Error: " Prelude.++ show err
                   error $ show err
    Right s  -> return s

-- | Produce a specification from the expression for a stream.
addStream :: VisualSettings -> String -> String -> IO Core.Spec
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
addStream' :: VisualSettings -> String -> String -> HI.Interpreter Core.Spec
addStream' settings name expr = do
  HI.setImportsQ (visualSettingsImports settings)
  let completeExpr = concat [ "observer ", show name, " (", expr, ")" ]

  spec <- HI.interpret completeExpr (HI.as :: Spec)
  HI.liftIO $ reify spec

data Trace = Trace
  { traceMap :: [ (String, UValues) ]
  }

data UValues = forall a . Typeable a => UValues
  { uvType   :: Core.Type a
  , uvValues :: [ a ]
  }

-- | Extract a trace from a core specification.
extractTrace :: Core.Spec -> Trace
extractTrace spec = Trace $ concat $ concat
  [ fmap extractTraceStream (Core.specStreams spec)
  , fmap extractTraceObserver (Core.specObservers spec)
  , fmap extractTraceTrigger (Core.specTriggers spec)
  ]

-- | Extract the values of a core 'Stream'.
extractTraceStream :: Core.Stream -> [ (String, UValues) ]
extractTraceStream (Core.Stream _id _buf expr _ty) =
  extractTraceExpr expr

-- | Extract the values of a core 'Observer'.
extractTraceObserver :: Core.Observer -> [ (String, UValues) ]
extractTraceObserver (Core.Observer _name expr _ty) =
  extractTraceExpr expr

-- | Extract the values of a core 'Trigger'.
extractTraceTrigger :: Core.Trigger -> [ (String, UValues) ]
extractTraceTrigger (Core.Trigger _name expr args) = concat $
    extractTraceExpr expr
  : fmap extractTraceUExpr args

-- | Extract the values of a core 'Expr'.
extractTraceExpr :: Core.Expr a -> [ (String, UValues) ]
extractTraceExpr (Core.Local _ _ _ expr1 expr2) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  ]
extractTraceExpr (Core.ExternVar ty name values) =
  [ (name, UValues ty (fromMaybe [] values)) ]
extractTraceExpr (Core.Op1 _op expr) =
  extractTraceExpr expr
extractTraceExpr (Core.Op2 _op expr1 expr2) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  ]
extractTraceExpr (Core.Op3 _op expr1 expr2 expr3) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  , extractTraceExpr expr3
  ]
extractTraceExpr (Core.Label _ty _lbl expr) =
  extractTraceExpr expr
extractTraceExpr _ = []

-- | Extract the values of a core 'UExpr'.
extractTraceUExpr :: Core.UExpr -> [ (String, UValues) ]
extractTraceUExpr (Core.UExpr ty expr) = extractTraceExpr expr

-- | Update values the values of a core 'UExpr'.
updateWithTrace :: Trace -> Core.Spec -> Core.Spec
updateWithTrace trace spec = spec
  { Core.specStreams =
      fmap (updateWithTraceStream trace) (Core.specStreams spec)
  , Core.specObservers =
      fmap (updateWithTraceObserver trace) (Core.specObservers spec)
  , Core.specTriggers =
      fmap (updateWithTraceTrigger trace) (Core.specTriggers spec)
  }

-- | Update the values of a 'Stream' based on an input trace.
updateWithTraceStream :: Trace -> Core.Stream -> Core.Stream
updateWithTraceStream trace (Core.Stream ident buf expr ty) =
  Core.Stream ident buf (updateWithTraceExpr trace expr) ty

-- | Update the values of an 'Observer' based on an input trace.
updateWithTraceObserver :: Trace -> Core.Observer -> Core.Observer
updateWithTraceObserver trace (Core.Observer name expr ty) =
  Core.Observer name (updateWithTraceExpr trace expr) ty

-- | Update the values of a 'Trigger' based on an input trace.
updateWithTraceTrigger :: Trace -> Core.Trigger -> Core.Trigger
updateWithTraceTrigger trace (Core.Trigger name expr args) =
  Core.Trigger
    name
    (updateWithTraceExpr trace expr)
    (fmap (updateWithTraceUExpr trace) args)

-- | Update the values of an 'Expr' based on an input trace.
updateWithTraceExpr :: Trace -> Core.Expr a -> Core.Expr a
updateWithTraceExpr trace (Core.Local ty1 ty2 name expr1 expr2) =
  Core.Local
    ty1
    ty2
    name
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
updateWithTraceExpr trace (Core.ExternVar ty name values) =
    Core.ExternVar ty name values'
  where
    values' | Just (UValues ty2 vals) <- lookup name (traceMap trace)
            , Just DE.Refl <- DE.testEquality ty ty2
            = Just vals
            | otherwise
            = values
updateWithTraceExpr trace (Core.Op1 op expr) =
  Core.Op1 op (updateWithTraceExpr trace expr)
updateWithTraceExpr trace (Core.Op2 op expr1 expr2) =
  Core.Op2 op
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
updateWithTraceExpr trace (Core.Op3 op expr1 expr2 expr3) =
  Core.Op3 op
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
    (updateWithTraceExpr trace expr3)
updateWithTraceExpr trace (Core.Label ty lbl expr) =
  Core.Label ty lbl (updateWithTraceExpr trace expr)
updateWithTraceExpr trace x = x

-- | Update the values of a 'UExpr' based on an input trace.
updateWithTraceUExpr :: Trace -> Core.UExpr -> Core.UExpr
updateWithTraceUExpr trace (Core.UExpr ty expr) =
  Core.UExpr ty (updateWithTraceExpr trace expr)
