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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Wire.API.MakesFederatedCall
  ( CallsFed,
    CallsAPI,
    MakesFederatedCall,
    FederatedComponent (..),
    makesCall,
    unsafeMakesCall,
  )
where

import Data.Aeson (Value (..))
import Data.Constraint
import Data.Metrics.Servant
import Data.Proxy
import Data.Swagger.Operation (addExtensions)
import qualified Data.Text as T
import GHC.TypeLits
import Imports
import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger
import Test.QuickCheck (Arbitrary)
import Unsafe.Coerce (unsafeCoerce)
import Wire.Arbitrary (GenericUniform (..))

data LocalComponent
  = InternalBrig
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform LocalComponent)

data FederatedComponent
  = Brig
  | Galley
  | Cargohold
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform FederatedComponent)

class MakesCall loc (comp :: loc) (name :: Symbol)

-- | A typeclass corresponding to calls to federated services. This class has
-- no methods, and exists only to automatically propagate information up to
-- servant.
--
-- The only way to discharge this constraint is via 'callsFed', which should be
-- invoked for each federated call when connecting handlers to the server
-- definition.
type CallsFed = MakesCall FederatedComponent
type CallsAPI = MakesCall LocalComponent


-- | A typeclass with the same layout as 'CallsFed', which exists only so we
-- can discharge 'CallsFeds' constraints by unsafely coercing this one.
class Nullary

instance Nullary

-- | Construct a dictionary for 'CallsFed'.
synthesizeMakesCall :: forall loc (comp :: loc) (name :: Symbol). Dict (MakesCall loc comp name)
synthesizeMakesCall = unsafeCoerce $ Dict @Nullary

-- | Servant combinator for tracking calls to federated calls. Annotating API
-- endpoints with 'MakesFederatedCall' is the only way to eliminate 'CallsFed'
-- constraints on handlers.
data MakesAPICall loc (comp :: loc) (name :: Symbol)

type MakesFederatedCall = MakesAPICall FederatedComponent

instance (HasServer api ctx) => HasServer (MakesAPICall loc comp name :> api :: *) ctx where
  -- \| This should have type @CallsFed comp name => ServerT api m@, but GHC
  -- complains loudly thinking this is a polytype. We need to introduce the
  -- 'CallsFed' constraint so that we can eliminate it via
  -- 'synthesizeCallsFed', which otherwise is too-high rank for GHC to notice
  -- we've solved our constraint.
  type ServerT (MakesAPICall loc comp name :> api) m = Dict (MakesCall loc comp name) -> ServerT api m
  route _ ctx f = route (Proxy @api) ctx $ fmap ($ synthesizeMakesCall @loc @comp @name) f
  hoistServerWithContext _ ctx f s = hoistServerWithContext (Proxy @api) ctx f . s

instance HasLink api => HasLink (MakesFederatedCall comp name :> api :: *) where
  type MkLink (MakesFederatedCall comp name :> api) x = MkLink api x
  toLink f _ l = toLink f (Proxy @api) l

instance RoutesToPaths api => RoutesToPaths (MakesFederatedCall comp name :> api :: *) where
  getRoutes = getRoutes @api

-- | Get a symbol representation of our component.
type family HeaderForLocation loc where
  HeaderForLocation FederatedComponent = "wire-makes-federated-call-to"
  HeaderForLocation LocalComponent = "wire-makes-api-call-to"

type ShowComponent :: forall loc -> loc -> Symbol
type family ShowComponent loc x where
  ShowComponent FederatedComponent 'Brig = "brig"
  ShowComponent FederatedComponent 'Galley = "galley"
  ShowComponent FederatedComponent 'Cargohold = "cargohold"
  ShowComponent LocalComponent 'InternalBrig = "brig"

-- | 'MakesFederatedCall' annotates the swagger documentation with an extension
-- tag @x-wire-makes-federated-calls-to@.
instance (HasSwagger api,
          KnownSymbol name,
          KnownSymbol (HeaderForLocation loc),
          KnownSymbol (ShowComponent loc comp)
         ) => HasSwagger (MakesAPICall loc comp name :> api :: *) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & addExtensions
        mergeJSONArray
        [ ( T.pack $ symbolVal $ Proxy @(HeaderForLocation loc),
            Array
              [ Array
                  [ String $ T.pack $ symbolVal $ Proxy @(ShowComponent loc comp),
                    String $ T.pack $ symbolVal $ Proxy @name
                  ]
              ]
          )
        ]

mergeJSONArray :: Value -> Value -> Value
mergeJSONArray (Array x) (Array y) = Array $ x <> y
mergeJSONArray _ _ = error "impossible! bug in construction of federated calls JSON"

instance HasClient m api => HasClient m (MakesAPICall loc comp name :> api :: *) where
  type Client m (MakesAPICalt l loc comp name :> api) = Client m api
  clientWithRoute p _ = clientWithRoute p $ Proxy @api
  hoistClientMonad p _ f c = hoistClientMonad p (Proxy @api) f c

-- | Type class to automatically lift a function of the form @(c1, c2, ...) =>
-- r@ into @Dict c1 -> Dict c2 -> ... -> r@.
class SolveMakesCall c r a where
  -- | Safely discharge a 'CallsFed' constraint. Intended to be used when
  -- connecting your handler to the server router.
  makesCall :: (c => r) -> a

instance (c ~ ((k, d) :: Constraint), SolveMakesCall d r a) => SolveMakesCall c r (Dict k -> a) where
  makesCall f Dict = makesCall @d @r @a f

instance {-# OVERLAPPABLE #-} (c ~ (() :: Constraint), r ~ a) => SolveMakesCall c r a where
  makesCall f = f

-- | Unsafely discharge a 'CallsFed' constraint. Necessary for interacting with
-- wai-routes.
--
-- This is unsafe in the sense that it will drop the 'CallsFed' constraint, and
-- thus might mean a federated call gets forgotten in the documentation.
unsafeMakesCall :: forall loc (comp :: loc) (name :: Symbol) r. (MakesCall loc comp name => r) -> r
unsafeMakesCall f = withDict (synthesizeMakesCall @loc @comp @name) f
