module Wire.Sem.Concurrency.Sequential where

import Imports
import Polysemy
import Wire.Sem.UnsafeConcurrency

sequentiallyPerformConcurrency :: Sem (Concurrency safe ': r) a -> Sem r a
sequentiallyPerformConcurrency = interpretH $ \case
  UnsafePooledMapConcurrentlyN _ f t -> do
    st <- getInitialStateT
    ftraverse <- bindT $ traverse @[] f
    raise $ sequentiallyPerformConcurrency $ ftraverse $ toList t <$ st
  UnsafePooledMapConcurrentlyN_ _ f (t :: t x) -> do
    st <- getInitialStateT
    ftraverse_ <- bindT $ traverse_ @t f
    raise $ sequentiallyPerformConcurrency $ ftraverse_ $ t <$ st
