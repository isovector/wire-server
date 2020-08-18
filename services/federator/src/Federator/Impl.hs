{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.Impl
  ( app,
  )
where

import Control.Monad.Catch (throwM)
import Data.Domain (Domain, domainText)
import Data.Handle (Handle)
import Data.Id (ConvId, UserId, makeIdOpaque, randomId)
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import qualified Federator.API as API
import Federator.App (AppIO, runAppT)
import Federator.Error (remoteBackendNotFound)
import qualified Federator.Remote as Remote
import Federator.Types (Env)
import Imports
import Network.DNS.Resolver as DNS
import Network.Wai (Application)
import Servant.Client.Core (BaseUrl (..), Scheme (Https))
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT, genericServeT)
import qualified Wire.API.Federation.API.Conversation as Fed
import qualified Wire.API.Federation.Event as Fed
import Wire.API.User.Client.Prekey (PrekeyBundle (PrekeyBundle))
import qualified Wire.Network.DNS.SRV as Srv

app :: Env -> Application
app env = genericServeT nat api
  where
    nat :: AppIO a -> Handler a
    nat = liftIO . runAppT env

api :: API.Api (AsServerT AppIO)
api =
  API.Api
    { _gapiPrekeys,
      _gapiJoinConversationById
    }

-- | dummy implementation
_gapiPrekeys :: Qualified UserId -> AppIO PrekeyBundle
_gapiPrekeys qualifiedUser = do
  pure $ PrekeyBundle (unsafeMakeOpaque qualifiedUser) []
  where
    -- FUTUREWORK(federation):
    -- this is unsafe, we should use the qualified ID (once the API type allows it)
    unsafeMakeOpaque = makeIdOpaque . _qLocalPart

-- | dummy implementation
_gapiJoinConversationById :: Qualified ConvId -> Fed.JoinConversationByIdRequest -> AppIO (Fed.ConversationUpdateResult Fed.MemberJoin)
_gapiJoinConversationById conversation request = do
  withRemoteBackend (_qDomain conversation) $ \baseUrl ->
    Remote.joinConversationById baseUrl conversation request

-- FUTUREWORK(federation): this needs a proper implementation
-- TODO(federation): add some special cases pointing to localhost for testing?
withRemoteBackend :: Domain -> (BaseUrl -> AppIO a) -> AppIO a
withRemoteBackend domainName action =
  liftIO resolve >>= \case
    Just (match : _) ->
      -- FUTUREWORK: we could retry with multiple matches instead of just the first
      action (toBaseUrl match)
    _ ->
      throwM $ remoteBackendNotFound domainName
  where
    -- TODO(federation): make resolv conf configurable?
    resolve = do
      -- TODO: I'm not sure what to call here anymore after rebasing onto
      -- https://github.com/wireapp/wire-server/pull/1177/files.
      -- I don't want to switch everything to Polysemy right now and also it
      -- doesn't seem like the implementation doesn't use orderSrvResult anymore?
      --
      -- previously was:
      --
      -- resolvSeed <- DNS.makeResolvSeed DNS.defaultResolvConf
      -- Srv.srvLookup (domainText domainName) resolvSeed
      undefined
    toBaseUrl :: Srv.SrvTarget -> BaseUrl
    toBaseUrl (Srv.SrvTarget domain port) =
      -- TODO(federation): extract this and test it thoroughly, based on docs for
      -- https://www.stackage.org/haddock/lts-14.27/dns/Network-DNS-Types.html#t:Domain
      -- TODO: remove trailing dot?
      -- TODO: should we allow custom ports?
      BaseUrl
        { baseUrlScheme = Https,
          -- TODO: avoid partial decodeUtf8, Text vs ByteString...
          baseUrlHost = Text.unpack (Text.E.decodeUtf8 domain),
          baseUrlPort = fromIntegral port,
          baseUrlPath = ""
        }