{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Brig.Effects.StompListener where

import Imports
import Data.Aeson (FromJSON, ToJSON)
import Polysemy
import qualified Brig.Queue.Stomp as Stomp
import Polysemy.Final
import System.Logger.Class (MonadLogger, log)
import Brig.Queue.Stomp (Broker)

data StompListener m a where
  Listen :: (Show msg, FromJSON msg) => (msg -> m ()) -> StompListener m ()
  Enqueue :: ToJSON a => a -> StompListener m ()

makeSem ''StompListener

instance MonadLogger IO where
  log _ _ = pure ()

toSTOMP :: Member (Final IO) r => Broker -> Text -> Sem (StompListener ': r) a -> Sem r a
toSTOMP b t = interpretFinal $ \case
   Listen f -> do
     st <- getInitialStateS
     f' <- bindS f
     -- TODO(sandy): How do we do this while still delegating logging?
     liftS $ Stomp.listen b t $ void . f' . (<$ st)
   Enqueue a -> liftS $ Stomp.enqueue b t a


