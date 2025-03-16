{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception            (handle, SomeException(..), finally)
import           Control.Monad
import qualified Copilot.Core                 as Core
import           Copilot.Interpret.Eval
import           Copilot.Language             hiding (interpret, typeOf)
import qualified Copilot.Language
import qualified Copilot.Visualize            as View
import           Data.Aeson
import           Data.List                    hiding ((++))
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as T
import qualified Data.Type.Equality           as DE
import           Data.Typeable
import           GHC.Generics
import           Language.Copilot
import           Language.Copilot             hiding (interpret, typeOf)
import qualified Language.Haskell.Interpreter as HI
import qualified Language.Haskell.Interpreter.Unsafe as HI
import qualified Network.WebSockets           as WS
import           Prelude                      hiding (div, not, (++), (<), (>))
import qualified Prelude
import           System.Directory
import           Text.Read

main :: IO ()
main = do
  putStrLn "WebSocket server starting on port 9160..."
  WS.runServer "127.0.0.1" 9160 app

makeTraceEval' k spec' =
  View.makeTraceEval k spec' (eval Haskell k spec')

-- * App

app :: WS.ServerApp
app pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $
      handle (\(e :: SomeException) -> do putStrLn $ "Something went wrong:" Prelude.++ show e
                                          error $ show e) $ do
        spec' <- readSpec spec

        let k = 3

        specV <- newMVar spec
        v <- newMVar (k, spec')

        let a = makeTraceEval' k spec'
            samples = encode $ toJSON $ allSamples a
        WS.sendTextData conn samples

        loop conn v specV

  where

    loop conn v specV = forever $ do
      msg <- WS.receiveData conn
      print msg
      let pair = readMaybe (T.unpack msg)
      print pair

      (k, spec') <- takeMVar v
      spec'' <- case (T.unpack msg, pair) of
                  ("StepUp", _)   -> pure spec'
                  ("StepDown", _) -> pure spec'
                  (_, Just (AddStream name expr, _)) -> do
                     spec <- takeMVar specV
                     let specN = spec Prelude.++ "\n" Prelude.++
                                 "      " Prelude.++ completeExpr
                         completeExpr = concat [ "observer "
                                               , show name
                                               , " ("
                                               , expr
                                               , ")"
                                               ]
                     let trace = extractTrace spec'
                     spec2 <- readSpec specN
                     putMVar specV specN
                     let spec3 = updateWithTrace trace spec2
                     return spec3
                  (_, Just (command, name)) -> apply spec' name command
                  _ -> pure spec'

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
    { teName      :: String
    , teIsBoolean :: Bool
    , teValues    :: [Sample]
    }
  deriving (Generic)

instance ToJSON TraceElem

data Sample = Sample
    { time     :: Int
    , value    :: String
    , duration :: Float
    }
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
             | AddStream String String
             | Noop
  deriving (Eq, Read, Show)

apply :: Core.Spec -> String -> Command -> IO Core.Spec
apply spec name (AddStream sName sExpr) = do
  putStrLn "Here"
  spec' <- addStream sName sExpr
  putStrLn "Here 2"
  -- TODO: I need to bring the streams from the other spec too, otherwise the
  -- streams to include may refer to streams by ID that are in a different
  -- scope.
  let observers' = Core.specObservers spec'
      observers  = Core.specObservers spec
  return $ spec { Core.specObservers = observers Prelude.++ observers' }

apply spec name command = pure $ spec
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

spec :: String
spec = unlines
  [ "let temperature :: Stream Word8"
  , "    temperature = extern \"temperature\" (Just [0, 15, 20, 25, 30])"
  , ""
  , "    ctemp :: Stream Float"
  , "    ctemp = (unsafeCast temperature) * (150.0 / 255.0) - 50.0"
  , ""
  , "    trueFalse :: Stream Bool"
  , "    trueFalse = [True] ++ not trueFalse"
  , ""
  , "in do trigger \"heaton\"  (temperature < 18) [arg ctemp, arg (constI16 1), arg trueFalse]"
  , "      trigger \"heatoff\" (temperature > 21) [arg (constI16 1), arg ctemp]"
  , "      observer \"temperature\" temperature"
  , "      observer \"temperature2\" (temperature + 1)"
  ]

spec' :: String -> HI.Interpreter Core.Spec
spec' spec = do
  HI.setImportsQ [ ("Prelude", Just "P")
                 , ("Copilot.Language", Nothing)
                 , ("Copilot.Language.Spec", Nothing)
                 , ("Language.Copilot", Nothing)
                 , ("Data.Functor.Identity", Nothing)
                 , ("Control.Monad.Writer", Nothing)
                 ]

  spec' <- HI.interpret spec (HI.as :: Spec)
  HI.liftIO $ reify spec'

readSpec :: String -> IO Core.Spec
readSpec spec = do
  r <- HI.runInterpreter (spec' spec)
  case r of
    Left err -> do putStrLn $ "There was an error, and here it is: " Prelude.++ show err
                   error $ show err
    Right s  -> return s

addStream :: String -> String -> IO (Core.Spec)
addStream name expr = do
  r <- HI.runInterpreter (addStream' name expr)
  case r of
    Left err   -> do putStrLn $ "There was an error, and here it is: " Prelude.++ show err
                     error $ show err
    Right spec -> return spec

-- observe that Interpreter () is an alias for InterpreterT IO ()
addStream' :: String -> String -> HI.Interpreter Core.Spec
addStream' name expr = do
  HI.setImportsQ [ ("Prelude", Just "P")
                 , ("Copilot.Language", Nothing)
                 , ("Copilot.Language.Spec", Nothing)
                 , ("Language.Copilot", Nothing)
                 , ("Data.Functor.Identity", Nothing)
                 , ("Control.Monad.Writer", Nothing)
                 ]

  -- For debugging purposes only: let completeExpr = "observer \"h1\" (constF 3.0)"
  let completeExpr = concat [ "observer "
                            , show name
                            , " ("
                            , expr
                            , ")"
                            ]

  -- HI.liftIO $ putStrLn $ "I'm about to interpret " ++ completeExpr
  spec <- HI.interpret completeExpr (HI.as :: Spec)
  -- HI.liftIO $ putStrLn "completed"
  HI.liftIO $ reify spec

data Trace = Trace
  { traceMap :: [ (String, UValues) ]
  }

data UValues = forall a . Typeable a => UValues
  { uvType   :: Core.Type a
  , uvValues :: [ a ]
  }

extractTrace :: Core.Spec -> Trace
extractTrace spec = Trace $ concat $ concat
  [ fmap extractTraceStream (Core.specStreams spec)
  , fmap extractTraceObserver (Core.specObservers spec)
  , fmap extractTraceTrigger (Core.specTriggers spec)
  ]

extractTraceStream :: Core.Stream -> [ (String, UValues) ]
extractTraceStream (Core.Stream _id _buf expr _ty) =
  extractTraceExpr expr

extractTraceObserver :: Core.Observer -> [ (String, UValues) ]
extractTraceObserver (Core.Observer _name expr _ty) =
  extractTraceExpr expr

extractTraceTrigger :: Core.Trigger -> [ (String, UValues) ]
extractTraceTrigger (Core.Trigger _name expr args) = concat $
    extractTraceExpr expr
  : fmap extractTraceUExpr args

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

extractTraceUExpr :: Core.UExpr -> [ (String, UValues) ]
extractTraceUExpr (Core.UExpr ty expr) =
  extractTraceExpr expr

updateWithTrace :: Trace -> Core.Spec -> Core.Spec
updateWithTrace trace spec = spec
  { Core.specStreams = fmap (updateWithTraceStream trace) (Core.specStreams spec)
  , Core.specObservers = fmap (updateWithTraceObserver trace) (Core.specObservers spec)
  , Core.specTriggers = fmap (updateWithTraceTrigger trace) (Core.specTriggers spec)
  }

updateWithTraceStream :: Trace -> Core.Stream -> Core.Stream
updateWithTraceStream trace (Core.Stream ident buf expr ty) =
  Core.Stream ident buf (updateWithTraceExpr trace expr) ty

updateWithTraceObserver :: Trace -> Core.Observer -> Core.Observer
updateWithTraceObserver trace (Core.Observer name expr ty) =
  Core.Observer name (updateWithTraceExpr trace expr) ty

updateWithTraceTrigger :: Trace -> Core.Trigger -> Core.Trigger
updateWithTraceTrigger trace (Core.Trigger name expr args) =
  Core.Trigger name (updateWithTraceExpr trace expr) (fmap (updateWithTraceUExpr trace) args)

updateWithTraceExpr :: Trace -> Core.Expr a -> Core.Expr a
updateWithTraceExpr trace (Core.Local ty1 ty2 name expr1 expr2) =
  Core.Local ty1 ty2 name (updateWithTraceExpr trace expr1) (updateWithTraceExpr trace expr2)
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
  Core.Op2 op (updateWithTraceExpr trace expr1) (updateWithTraceExpr trace expr2)
updateWithTraceExpr trace (Core.Op3 op expr1 expr2 expr3) =
  Core.Op3 op
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
    (updateWithTraceExpr trace expr3)
updateWithTraceExpr trace (Core.Label ty lbl expr) =
  Core.Label ty lbl (updateWithTraceExpr trace expr)
updateWithTraceExpr trace x = x

updateWithTraceUExpr :: Trace -> Core.UExpr -> Core.UExpr
updateWithTraceUExpr trace (Core.UExpr ty expr) =
  Core.UExpr ty (updateWithTraceExpr trace expr)
