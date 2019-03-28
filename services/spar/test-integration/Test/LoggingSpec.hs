module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Data.String.Conversions (cs)
import Network.HTTP.Types.Status (statusCode)
import Spar.App
import Spar.Run (mkApp)
import System.IO.Silently (capture)
import System.Logger as Log
import Util

import qualified Network.Wai.Test as HW
import qualified Test.Hspec.Wai as HW
import qualified Test.Hspec.Wai.Internal as HW


spec :: SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines (see haddocks of simpleSettings)" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ do
        Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: Text)
        Log.flush logger
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"

  context "loglevel == debug" $ do
    it "400 on finalize-login causes log of entire request" $ do
      (app, env) <- liftIO . mkApp =<< view teOpts
      let badbody = "@@badxml"
      (out, resp) <- liftIO . capture $ do
        resp <- HW.withApplication app $ HW.post "/sso/finalize-login" badbody
        Log.flush (sparCtxLogger env)
        pure resp
      liftIO $ do
        statusCode (HW.simpleStatus resp) `shouldBe` 400
        out `shouldContain` cs badbody
