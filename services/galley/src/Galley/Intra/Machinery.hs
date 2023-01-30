module Galley.Intra.Machinery
  ( rpcClient
  , doAPICall
  , RpcComponent (..)
  , runAPICall
  ) where

import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.String.Conversions
import Galley.API.Error
import Galley.Env
import Galley.Monad
import Imports
import qualified Servant.Client as Client
import Util.Options
import Wire.API.RPC

doAPICall :: HasCallStack => Client.ClientM a -> App (Either Client.ClientError a)
doAPICall action = do
  mgr <- view manager
  brigep <- view brig
  let env = Client.mkClientEnv mgr baseurl
      baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. epHost) (fromIntegral $ brigep ^. epPort) ""
  liftIO $ Client.runClientM action env

runAPICall :: HasCallStack => Client.ClientM a -> App a
runAPICall action = doAPICall action >>= handleServantResp

handleServantResp ::
  Either Client.ClientError a ->
  App a
handleServantResp (Right cfg) = pure cfg
handleServantResp (Left errmsg) = throwM . internalErrorWithDescription . cs . show $ errmsg

