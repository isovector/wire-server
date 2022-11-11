{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Brig.Effects.StompListener where

import Imports
import Data.Aeson (ToJSON (toJSON), Value)
import Polysemy
import qualified Brig.Queue as Queue
import Polysemy.Final
import System.Logger.Class (MonadLogger, log)
import Brig.App (Env)
import qualified Polysemy.Reader as P
import Unsafe.Coerce (unsafeCoerce)


data StompListener m a where
  ListenDuring :: (Value -> m ()) -> m a -> StompListener m a
  Enqueue :: Value -> StompListener m ()

makeSem ''StompListener

instance MonadLogger IO where
  log _ _ = pure ()

toSTOMP :: Member (Final IO) r => Env -> Queue.Queue -> Sem (StompListener ': r) a -> Sem r a
toSTOMP env q = interpretFinal $ \case
   ListenDuring f m -> do
     st <- getInitialStateS
     f' <- bindS f
     -- TODO(sandy): How do we do this while still delegating logging?
     _ <- liftS $ do
       flip runReaderT env $ Queue.listen q $ void . lift . f' . (<$ st)
     runS m
   Enqueue a -> liftS $ flip runReaderT env $ Queue.enqueue q a

data PureCont where
  PureCont :: (f Value -> m (f ())) -> PureCont

pureSTOMP0 :: Sem (StompListener ': r) a -> Sem r a
pureSTOMP0  = P.runReader (PureCont $ error "enqueued without calling listen") . pureSTOMP1

pureSTOMP1 :: Sem (StompListener ': r) a -> Sem (P.Reader PureCont ': r) a
pureSTOMP1  = reinterpretH @_ @(P.Reader PureCont) \case
  ListenDuring f m -> do
    f' <- bindT f
    m' <- runT m
    P.local (const $ PureCont $ raise . f') $ raise $ subsume $ pureSTOMP1 m'
  Enqueue msg -> do
    PureCont z <- P.ask
    st <- getInitialStateT
    -- Unfortunately, we need to use 'unsafeCoerce' here to get our callback
    -- back into the right shape. This is impossible to type properly, because
    -- the 'R.Reader' we're asking needs to exist before the weaved state.
    --
    -- It sucks, but it works, and this is just for test code anyway.
    unsafeCoerce z $ toJSON msg <$ st

main :: IO ()
main = runM $ pureSTOMP0 $ do
  listenDuring (\z -> embed @IO $ print z) $ do
    enqueue $ toJSON @String "hello"
    enqueue $ toJSON @String "hello"
    enqueue $ toJSON @String "hello"
    enqueue $ toJSON @String "hello"

