module Galley.Intra.Machinery
  ( rpcClient
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

runAPICall :: HasCallStack => Client.ClientM a -> App a
runAPICall action = do
  mgr <- view manager
  brigep <- view brig
  let env = Client.mkClientEnv mgr baseurl
      baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. epHost) (fromIntegral $ brigep ^. epPort) ""
  liftIO (Client.runClientM action env) >>= handleServantResp

handleServantResp ::
  Either Client.ClientError a ->
  App a
handleServantResp (Right cfg) = pure cfg
handleServantResp (Left errmsg) = throwM . internalErrorWithDescription . cs . show $ errmsg

