{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception       ( finally )
import           Control.Monad
import qualified Copilot.Core            as Core
import           Copilot.Interpret.Eval
import qualified Copilot.Visualize       as View
import           Data.Aeson
import qualified Data.Text               as T
import           Data.Typeable
import           GHC.Generics
import           Language.Copilot
import qualified Network.WebSockets      as WS
import           Prelude                 hiding ( div, not, (++), (<), (>) )
import qualified Prelude

main :: IO ()
main = do
  putStrLn "WebSocket server starting on port 9160..."
  WS.runServer "127.0.0.1" 9160 (app spec)

makeTraceEval' k spec' =
  View.makeTraceEval k spec' (eval Haskell k spec')

-- * App

app :: Spec -> WS.ServerApp
app spec pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do
      spec' <- reify spec

      let k = 3

      v <- newMVar (k, spec')

      let a = makeTraceEval' k spec'
          samples = encode $ toJSON $ allSamples a
      WS.sendTextData conn samples

      loop conn v

  where

    loop conn v = forever $ do
      msg <- WS.receiveData conn
      let (command, name) =
            case T.unpack msg of
              ('U':'p':' ':d:' ':s)         -> (Up (read [d]), s)
              ('D':'o':'w':'n':' ':d:' ':s) -> (Down (read [d]), s)
              v                             -> (Noop, v)

      (k, spec') <- takeMVar v
      let spec'' = case T.unpack msg of
                     "StepUp"   -> spec'
                     "StepDown" -> spec'
                     _          -> apply spec' name command

      let k'     = case T.unpack msg of
                     "StepUp"   -> k + 1
                     "StepDown" -> k - 1
                     _          -> k

      putMVar v (k', spec'')

      let a       = makeTraceEval' k' spec''
          samples = encode $ toJSON $ allSamples a
      WS.sendTextData conn samples

data Data = Data
    { adLastSample :: Int
    , adTraceElems :: [TraceElem]
    }
  deriving (Generic)

instance ToJSON Data

data TraceElem = TraceElem
    { teName :: String
    , teIsBoolean :: Bool
    , teValues :: [Sample]
    }
  deriving (Generic)

instance ToJSON TraceElem

data Sample = Sample
    { time :: Int, value :: String, duration :: Float }
  deriving (Generic)

instance ToJSON Sample

allSamples :: View.AppData -> Data
allSamples appData = Data
  { adLastSample = View.adLastSample appData
  , adTraceElems = map toTraceElem (View.adTraceElems appData)
  }

toTraceElem :: View.TraceElem -> TraceElem
toTraceElem te = TraceElem
  { teName      = View.teName te
  , teIsBoolean = View.teIsBoolean te
  , teValues    = map
                    (\(i, v) -> Sample i (View.tvValue v) 1)
                    (zip [0..] (View.teValues te))
  }

-- * Update specs using commands

data Command = Up Int
             | Down Int
             | Noop
  deriving (Eq, Read, Show)

apply :: Core.Spec -> String -> Command -> Core.Spec
apply spec name command = spec
  { Core.specStreams =
      map (updateStream name command) (Core.specStreams spec)
  , Core.specObservers =
      map (updateObserver name command) (Core.specObservers spec)
  , Core.specTriggers =
      map (updateTrigger name command) (Core.specTriggers spec)
  }

updateObserver :: String -> Command -> Core.Observer -> Core.Observer
updateObserver name command (Core.Observer i e ty) = (Core.Observer i e' ty)
  where
    e' = updateExpr name command e

updateTrigger :: String -> Command -> Core.Trigger -> Core.Trigger
updateTrigger name command (Core.Trigger i e es) = (Core.Trigger i e' es')
  where
    e'  = updateExpr name command e
    es' = map (updateUExpr name command) es

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

updateUExpr :: String -> Command -> Core.UExpr -> Core.UExpr
updateUExpr name cmd (Core.UExpr ty e) = Core.UExpr ty (updateExpr name cmd e)

updateStream :: String -> Command -> Core.Stream -> Core.Stream
updateStream name command (Core.Stream i b e ty) =
  (Core.Stream i b (updateExpr name command e) ty)

updateValues :: Maybe [a] -> Type a -> Command -> Maybe [a]
updateValues vsM ty command =
  fmap (\vs -> fmap (updateValue command ty) (zip [0..] vs)) vsM

updateValue :: Command -> Type a -> (Int, a) -> a
updateValue (Up n)   Core.Bool  (ix, a) = if n Prelude.== ix then Prelude.not a else a
updateValue (Down n) Core.Bool  (ix, a) = if n Prelude.== ix then Prelude.not a else a
updateValue (Up n)   Core.Word8 (ix, a) = if n Prelude.== ix then a + 1 else a
updateValue (Down n) Core.Word8 (ix, a) = if n Prelude.== ix then a - 1 else a
updateValue _        _          (ix, a) = a

-- * Sample spec

-- External temperature as a byte, range of -50C to 100C
temp :: Stream Word8
temp = extern "temperature" (Just [0, 15, 20, 25, 30])

-- Calculate temperature in Celsius.
-- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as there
-- is no direct relation between Word8 and Float.
ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

trueFalse :: Stream Bool
trueFalse = [True] ++ not trueFalse

spec = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (temp < 18) [arg ctemp, arg (constI16 1), arg trueFalse]
  trigger "heatoff" (temp > 21) [arg (constI16 1), arg ctemp]
  observer "temperature" temp
  observer "temperature2" (temp + 1)
