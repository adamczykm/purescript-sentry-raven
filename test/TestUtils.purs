module Test.TestUtils where

import Prim.Row (class Lacks)
import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind)
import Control.Category (identity)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.AVar (empty, put, tryRead)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import Foreign (Foreign)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe, maybe)
import Record (insert)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Sentry.Raven (Dsn(..))
import Sentry.Raven.Core (Dsn, withRaven)
import Sentry.Raven.Core.Internal (Raven)
import Simple.JSON (class ReadForeign, class WriteForeign)
-- import Type.Row (class RowLacks)

testRaven ∷
  ∀ ctx a
  . WriteForeign ctx
  ⇒ ReadForeign ctx
  ⇒ ctx
  → Maybe (a → Boolean)
  → (Maybe Foreign → Boolean)
  → (∀ h. Raven h ctx → Effect a)
  → Aff Boolean
testRaven ctx mValidateRes validate action =
  let validateRes = maybe (const true) identity mValidateRes
  in requestOutputTest (Dsn "") {} ctx (Milliseconds 500.0) validateRes validate action

requestOutputTest
  ∷ ∀ opts ctx a
  . Lacks "dataCallback" opts
  ⇒ WriteForeign ctx
  ⇒ Dsn
  → { | opts}
  → ctx
  → Milliseconds
  → (a → Boolean)
  → (Maybe Foreign → Boolean)
  → (∀ h. Raven h ctx → Effect a)
  → Aff Boolean
requestOutputTest dsn opts ctx timeout validateResult validateSentryRequest action = do
  sentryRequestVar ← empty
  ret ← liftEffect $ withRaven dsn
                  (insert (SProxy ∷ SProxy "dataCallback")
                          (updateWithGeneratedRequest sentryRequestVar)
                          opts)
                  ctx
                  action

  if not (validateResult ret) then pure false
    else delay timeout *> (validateSentryRequest <$> tryRead sentryRequestVar)

  where
    updateWithGeneratedRequest avar = (mkEffectFn1 (\x → launchAff_ (put x avar) *> pure x))
