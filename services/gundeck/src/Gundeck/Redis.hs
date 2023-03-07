{-# LANGUAGE NumDecimals                 #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeApplications            #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.Redis
  ( RobustConnection,
    rrConnection,
    rrReconnect,
    connectRobust,
    runRobust,
    PingException,
  )
where

import Control.Concurrent.Extra (once)
import Control.Lens
import qualified Control.Monad.Catch as Catch
import Control.Retry
import Database.Redis
import Gundeck.Redis.HedisExtensions
import Imports
import qualified System.Logger as Log
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as LogClass
import System.Logger.Extended
import UnliftIO.Exception
import Debug.Trace
import Control.Concurrent.Async (async)
import Control.Monad.Catch (MonadCatch)

-- | Connection to Redis which allows reconnecting.
type RobustConnection = MVar Connection

data ReConnection = ReConnection
  { -- | established (and potentially breaking) connection to Redis
    _rrConnection :: Connection,
    -- | action which can be called to reconnect to Redis
    _rrReconnect :: IO ()
  }

makeLenses ''ReConnection

-- | Connection to Redis which can be reestablished on connection errors.
--
-- Reconnecting even when Redis IPs change as long as the DNS name remains
-- constant. The server type (cluster or not) and the connection information of
-- the initial connection are used when reconnecting.
--
-- Throws 'ConnectError', 'ConnectTimeout', 'ConnectionLostException',
-- 'PingException', or 'IOException' if retry policy is finite.
connectRobust ::
  Logger ->
  -- | e. g., @exponentialBackoff 50000@
  RetryPolicy ->
  -- | action returning a fresh initial 'Connection', e. g., @(checkedConnect connInfo)@ or @(checkedConnectCluster connInfo)@
  IO Connection ->
  IO (MVar Connection)
connectRobust l retryStrategy connectLowLevel = do
  robustConnection <- newEmptyMVar @IO @Connection
  _ <-
    async $ safeForever $ do
      conn <- retry connectLowLevel
      putMVar robustConnection conn
      catch
        ( forever $ do
            _ <- runRedis conn ping
            threadDelay 1e6
        ) $ \(_ :: SomeException) -> void $ takeMVar robustConnection
  pure robustConnection
  -- retry $ reconnectRedis robustConnection
  -- pure robustConnection
  where
    retry =
      recovering -- retry connecting, e. g., with exponential back-off
        retryStrategy
        [ const $ Catch.Handler (\(e :: ClusterDownError) -> logEx (Log.err l) e "Redis cluster down" >> pure True),
          const $ Catch.Handler (\(e :: ConnectError) -> logEx (Log.err l) e "Redis not in cluster mode" >> pure True),
          const $ Catch.Handler (\(e :: ConnectTimeout) -> logEx (Log.err l) e "timeout when connecting to Redis" >> pure True),
          const $ Catch.Handler (\(e :: ConnectionLostException) -> logEx (Log.err l) e "Redis connection lost during request" >> pure True),
          const $ Catch.Handler (\(e :: PingException) -> logEx (Log.err l) e "pinging Redis failed" >> pure True),
          const $ Catch.Handler (\(e :: IOException) -> logEx (Log.err l) e "network error when connecting to Redis" >> pure True)
        ]
        . const -- ignore RetryStatus
  --   reconnectRedis robustConnection = do
  --     Log.info l $ Log.msg (Log.val "connecting to Redis")
  --     conn <- connectLowLevel
  --     Log.info l $ Log.msg (Log.val "successfully connected to Redis")

  --     reconnectOnce <- once $ do
  --       traceMarkerIO "disconnect"
  --       retry $ do
  --         disconnect conn
  --         reconnectRedis robustConnection -- avoid concurrent attempts to reconnect
  --           <* traceMarkerIO "reconnected"
  --     let newReConnection = ReConnection {_rrConnection = conn, _rrReconnect = reconnectOnce}
  --     unlessM (tryPutMVar robustConnection newReConnection) $
  --       void $
  --         swapMVar robustConnection newReConnection

    logEx :: Show e => ((Msg -> Msg) -> IO ()) -> e -> ByteString -> IO ()
    logEx lLevel e description = lLevel $ Log.msg (Log.val description) . Log.field "error" (show e)

-- | Run a 'Redis' action through a 'RobustConnection'.
--
-- Blocks on connection errors as long as the connection is not reestablished.
-- Without externally enforcing timeouts, this may lead to leaking threads.
runRobust :: (MonadUnliftIO m, MonadLogger m) => RobustConnection -> Redis a -> m a
runRobust mvar action = do
  robustConnection <- readMVar mvar
  liftIO $ runRedis (robustConnection) action
  -- catches
  --   ()
  --   [ logAndHandle $ Handler (\(_ :: ConnectionLostException) -> reconnectRetry robustConnection), -- Redis connection lost during request
  --     logAndHandle $ Handler (\(_ :: IOException) -> reconnectRetry robustConnection) -- Redis unreachable
  --   ]
  -- where
  --   reconnectRetry robustConnection = do
  --     liftIO $ do
  --       traceMarkerIO "robust reconnect"
  --       _rrReconnect robustConnection
  --     runRobust mvar action

  --   logAndHandle (Handler handler) =
  --     Handler $ \e -> do
  --       LogClass.err $ Log.msg (Log.val "Redis connection failed") . Log.field "error" (show e)
  --       handler e

data PingException = PingException Reply deriving (Show)

instance Exception PingException



safeForever ::
  forall m.
  (MonadUnliftIO m) =>
  m () ->
  m ()
safeForever action =
  forever $
    action `catchAny` \_ -> do
      threadDelay 60000000 -- pause to keep worst-case noise in logs manageable

