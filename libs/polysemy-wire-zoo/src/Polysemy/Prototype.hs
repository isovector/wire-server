{-# LANGUAGE AllowAmbiguousTypes         #-}
{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.Prototype where

import Polysemy
import Polysemy.State
import Imports hiding (read)
import Control.Arrow (first, second)
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal
import Polysemy.Internal.Union


data Tty m a where
  Read :: Tty m (Maybe String)
  Write :: String -> Tty m ()


makeSem ''Tty


runTty :: [String] -> Sem (Tty ': r) a -> Sem r (([String], [String]), a)
runTty ins a
  = fmap (first $ second reverse)
  $ runState (ins, [] :: [String])
  $ reinterpret (\case
      Read -> do
        r <- gets $ listToMaybe . fst
        modify $ first $ drop 1
        pure r
      Write s -> do
        modify $ second (s :)
                )
    a

replace
    :: ( Member e r
       , FirstOrder e "replace"
       )
    => (forall x rInitial. e (Sem rInitial) x -> Sem r x)
    -> Sem r a
    -> Sem r a
replace f = replaceH $ \(e :: e (Sem rInitial) x) ->
  liftT @(Sem rInitial) $ f e


replaceH
    :: Member e r
    => (forall x r0. e (Sem r0) x -> Tactical e (Sem r0) r x)
    -> Sem r a
    -> Sem r a
replaceH f (Sem m) = Sem $ \k -> m $ \u ->
  case prj u of
    Just (Weaving e s d y v) ->
      fmap y $ do
        void $ k u
        usingSem k
          $ runTactics s (raise . d) v (replaceH f . d)
          $ f e
    Nothing -> k $ hoist (replaceH f) u


tryInIO :: Member (Embed IO) r => Tty (Sem r0) x -> Sem r x
tryInIO = \case
  Read -> embed $ fmap Just $ fmap ("IO: " ++) getLine
  Write s -> embed $ putStrLn $ "(IO) " ++ s


main :: IO ()
main = (print =<<) $ runM $ runTty ["hello", "world"] $ replace tryInIO $ do
  read >>= \case
    Nothing -> do
      write "NO INPUT"
      pure "bad"
    Just s -> do
      write s
      pure @_ @String "ok"



