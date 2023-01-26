module Wire.API.RPC where

import GHC.TypeLits
import Servant.Client
import Wire.API.Routes.Named
import qualified Wire.API.Routes.Internal.Brig as IBrigRoutes


data RpcComponent
  = BrigInternal

type family RpcApi (comp :: RpcComponent) = (api :: *) | api -> comp where
  RpcApi 'BrigInternal = IBrigRoutes.API


-- | Return a client for a named endpoint.
rpcClient ::
  forall (comp :: RpcComponent) (name :: Symbol) m endpoint.
  (HasClient m endpoint, HasEndpoint (RpcApi comp) endpoint name) =>
  Client m endpoint
rpcClient = namedClient @(RpcApi comp) @name @m

