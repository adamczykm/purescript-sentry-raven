module Test.Main where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category (id, (<<<))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Eq (class Eq, (/=), (==))
import Data.Foreign (Foreign)
import Data.HeytingAlgebra (not, (&&))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Prelude (Unit, const, ($), (>>=), discard)
import Sentry.Raven (RIx(..), captureException, captureMessage, getContext, modifyUser, parseForeignNested', recordBreadcrumb, setUser, withAddedTags)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Test.TestUtils (testRaven)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Type.Data.Boolean (kind Boolean)


main ∷ ∀ eff. Eff
  ( console ∷ CONSOLE
  , testOutput ∷ TESTOUTPUT
  , avar ∷ AVAR
  | eff
  )
  Unit
main = runTest do
  suite "Capturing events" do

    testAssert "Should not generate an event" $
      testRaven {} Nothing notGenerated
      (\r → do
        recordBreadcrumb r sampleBreadcrumb)

    testAssert "Should capture message" $
      testRaven {} Nothing generated
      (\r → do
        captureMessage r "testMsg" {})

    testAssert "Should capture exception" $
      testRaven {} Nothing generated
      (\r → do
        captureException r "testError" {})

  suite "Context simple" do

    let userCtx =
          User { id: 1
               , username: "test"
               , email: "test@purssentryraven.org"
               , ip_address: "127.0.0.1"}

        userCtx2 =
          User { id: 2
               , username: "test2"
               , email: "test@purssentryraven.org"
               , ip_address: "127.0.0.1"}

        verifyUserContext ∷ User → Maybe Foreign → Boolean
        verifyUserContext u me = maybeBoolean do
          e ← me
          user ← parseForeignNested' (IxP "user" : Nil) e
          pure $ u == user

    testAssert "Starting user ctx is captured" $
      testRaven {user : userCtx} Nothing (verifyUserContext userCtx)
      (\r → do
        captureMessage r "testMsg" {})

    test "getContext works as expected" $ do
      ret ← testRaven {user : userCtx} (Just id) (const true)
              (\r → do
                ctx ← getContext r
                pure $ ctx.user == userCtx)
      Assert.assert "user context works" ret

      ret ← testRaven {custom : userCtx} (Just id) (const true)
              (\r → do
                ctx ← getContext r
                pure $ ctx.custom == userCtx)
      Assert.assert "custom context works" ret

    testAssert "Set user context works as expected" $
      testRaven {user : userCtx} Nothing (verifyUserContext userCtx2)
      (\r → do
        setUser r userCtx2
        captureMessage r "testMsg" {})

    testAssert "Modify user context works as expected" $
      testRaven {user : userCtx} Nothing (verifyUserContext userCtx2)
      (\r → do
        modifyUser r (const userCtx2)
        captureMessage r "testMsg" {})


    let
      verifyTags ∷ String → String → (Maybe Foreign) → Boolean
      verifyTags tagK tagV me = maybeBoolean do
          e ← me
          tagVal ← parseForeignNested' (IxP "tags" : IxP tagK : Nil) e
          pure $ tagVal == tagV

    testAssert "Tags adding works as expected" $
      testRaven {tags:{}} (Just id) (const true)
      (\r → withAddedTags r {tagKey: "tagValue"} (\rt → do
        captureMessage rt "testMsg" {}
        ctx ← getContext rt
        pure $ ctx.tags.tagKey == "tagValue"))


  where

    sampleBreadcrumb = { category : "Test"}

    testAssert msg t = test msg (t >>= Assert.assert msg)

    generated = maybe false (const true)

    notGenerated = not <<< generated

    maybeBoolean = case _ of
      Just x → x
      Nothing → false



newtype User = User
  { id ∷ Int
  , username ∷ String
  , email ∷ String
  , ip_address ∷ String }

derive newtype instance rfUserImpl ∷ ReadForeign User
derive newtype instance wfUserImpl ∷ WriteForeign User

instance eqUserImpl ∷ Eq User where
  eq (User u1) (User u2) =
    u1.id == u2.id &&
    u1.username == u2.username &&
    u1.email == u2.email &&
    u1.ip_address == u2.ip_address
