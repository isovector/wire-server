{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.Prototype where

import Control.Concurrent.MVar
import Control.Arrow (first, second)
import Imports hiding (tryTakeMVar, tryReadMVar, putMVar, read, intersperse)
import Polysemy
import Polysemy.Trace
import Polysemy.Internal
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union
import Polysemy.State
import Polysemy.Input (runInputConst, input, Input (Input))
import Polysemy.Output (runOutputSem, output)
import UnliftIO (async)

replace ::
  ( Member e r,
    FirstOrder e "replace"
  ) =>
  (forall x rInitial. e (Sem rInitial) x -> Sem r (Replace x)) ->
  Sem r a ->
  Sem r a
replace f = replaceH $ \(e :: e (Sem rInitial) x) ->
  liftT @(Sem rInitial) $ f e

data Replace a
  = Replace a
  | AlsoRunReplaced a
  deriving (Eq, Ord, Show, Functor)

unreplace :: Replace a -> a
unreplace (Replace a) = a
unreplace (AlsoRunReplaced a) = a

------------------------------------------------------------------------------

-- | 'replace \@e' intercepts actions in @e@ and runs the given handler on
-- them, effectively replacing one handler with another. For example, given the
-- following:
--
-- @@
-- runA :: Member B r => Sem (A ': r) x -> Sem r x
-- runB :: Sem (B ': r) x -> Sem r x
--
-- runComposed :: Sem (A ': B ': r) x -> Sem r x
-- runComposed = runB . runA
-- @@
--
-- This is common in real-world examples, where the effect stack is big and too
-- burdensome to inline at every call site. As a result, there are a handful of
-- canonical interpreters ala @runComposed@.
--
-- While this pattern is convenient for application authors, it's a pain for
-- authors of tests, who need to duplicate @runComposed@ and sub out some of
-- the real interpreters for mock interpreters.
--
-- Enter 'replace', which allows us to precompose on 'runComposed' to change
-- the interpretation of an effect already handled in the stack. For example,
-- given
--
-- @@
-- runB' :: B (Sem r0) x -> Sem r (Replace x)
-- @@
--
-- we can call @runComposed . replace runB'@ to effectively change the
-- interpretation from @runB@ to @interpret runB'@ --- however, this /EXCLUDES/
-- the actions in @B@ that are visible to @A@. Alas.
--
-- The @Replace@ type in the result allows replacing interpreters to choose
-- whether the replaced interpreter should also be run. Sometimes this is
-- desirable, such as when you are replacing a state-backed interpreter and
-- would still like the state changes to propagate.
--
-- The replacing handler for @e@ may still call actions in @e@, which will be
-- forwarded to the replaced handler. This is a useful escape hatch if the
-- @Replace@ type doesn't give you all the control you'd like.
replaceH ::
  Member e r =>
  (forall x r0. e (Sem r0) x -> Tactical e (Sem r0) r (Replace x)) ->
  Sem r a ->
  Sem r a
replaceH f (Sem m) = Sem $ \k -> m $ \u ->
  case prj u of
    Just (Weaving e s d y v) ->
      fmap y $ do
        z <-
          usingSem k $
            runTactics s (raise . d) v (replaceH f . d) $
              f e
        for_ (v z) $ \case
          Replace _ -> pure ()
          AlsoRunReplaced _ -> void $ k u
        pure $ fmap unreplace z
    Nothing -> k $ hoist (replaceH f) u



------------------------------------------------------------------------------

intersperse
  :: Sem r () ->
  Sem r a ->
  Sem r a
intersperse a (Sem m) =
  Sem $ \k -> m $ \u -> do
    usingSem k a
    k u


load :: (Member (Embed IO) r, Member Trace r) => MVar String -> Sem r ()
load chan = do
  embed (tryTakeMVar chan) >>= \case
    Nothing -> pure ()
    Just s -> do
      trace $ "loaded: " <> s
      load chan


magic :: MVar String -> IO ()
magic chan = do
  putMVar chan "hello"
  putMVar chan "world"
  putMVar chan "zoup"



main :: IO ()
main = do
  chan <- Control.Concurrent.MVar.newEmptyMVar @String
  _ <- async $ magic chan
  runM $
    traceToStdout $
      intersperse (load chan) $ do
        trace "1"
        trace "2"
        trace "3"
        trace "4"
