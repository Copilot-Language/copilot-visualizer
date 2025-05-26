{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Interact with a running Copilot simulation via a websocket.
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

-- External imports
import           Control.Exception      (SomeException (..), handle)
import qualified Copilot.Core           as Core
import           Copilot.Interpret.Eval (ShowType (Haskell), eval)
import           Copilot.Language       hiding (interpret, typeOf)
import qualified Copilot.Visualize.Tikz as View
import           Data.Aeson             (ToJSON (..), encode)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import qualified Network.WebSockets     as WS
import           Prelude                hiding (div, not, (++), (<), (>))
import qualified Prelude
import           Text.Read              (readMaybe)

-- Internal imports
import Copilot.Visualize.Dynamic

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
  { visualSettingsHost       :: String
                                -- ^ Host interface to listen to. Use
                                -- "127.0.0.1" to listen at localhost.

  , visualSettingsPort       :: Int
                                -- ^ Port to listen to.

  , visualSettingsSimulation :: SimulationSettings
                                -- ^ Settings for the simulation.
  }

-- | Default settings that simulates 3 steps and listens on localhost at port
-- 9160.
mkDefaultVisualSettings :: VisualSettings
mkDefaultVisualSettings = VisualSettings
  { visualSettingsHost       = "127.0.0.1"
  , visualSettingsPort       = 9160
  , visualSettingsSimulation = mkDefaultSimulationSettings
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
    simData  <- simInit (visualSettingsSimulation settings) spec

    -- Communicate the current values of the trace to the user interface.
    let appData = makeTraceEval' (simSteps simData) (simSpec simData)
    let samples = encode $ toJSON $ allSamples appData
    WS.sendTextData conn samples

    -- Start the application loop.
    appMainLoop settings conn simData

  where

    appException :: SomeException -> IO ()
    appException e = do
      putStrLn $ "Error:" Prelude.++ show e
      error $ show e

appMainLoop :: VisualSettings
            -> WS.Connection
            -> SimData
            -> IO ()
appMainLoop settings conn simData = do
  let simulationSettings = visualSettingsSimulation settings

  msg <- T.unpack <$> WS.receiveData conn

  let pair :: Maybe (Command, String)
      pair = readMaybe msg

  simData' <- simStep simulationSettings simData msg pair

  let appData = makeTraceEval' (simSteps simData') (simSpec simData')
      samples = encode $ toJSON $ allSamples appData
  WS.sendTextData conn samples

  appMainLoop settings conn simData'

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
