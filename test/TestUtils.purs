module Test.TestUtils where

import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind)
import Control.Category (id)
import Control.Monad.Aff (Aff, delay, launchAff_)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, tryTakeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Data.Foreign (Foreign)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe, maybe)
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (traceAnyA)
import Sentry.Raven (Dsn(..))
import Sentry.Raven.Core (Dsn, withRaven)
import Sentry.Raven.Core.Internal (RAVEN, Raven)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Row (class RowLacks)

testRaven ∷
  ∀ eff ctx a
  . WriteForeign ctx
  ⇒ ReadForeign ctx
  ⇒ ctx
  → Maybe (a → Boolean)
  → (Maybe Foreign → Boolean)
  → (∀ h. Raven h ctx → Eff (avar ∷ AVAR, raven ∷ RAVEN h | eff) a)
  → Aff (avar ∷ AVAR | eff) Boolean
testRaven ctx mValidateRes validate action =
  let validateRes = maybe (const true) id mValidateRes
  in requestOutputTest (Dsn "") {} ctx (Milliseconds 500.0) validateRes validate action

requestOutputTest
  ∷ ∀ opts eff ctx a
  . RowLacks "dataCallback" opts
  ⇒ WriteForeign ctx
  ⇒ Dsn
  → { | opts}
  → ctx
  → Milliseconds
  → (a → Boolean)
  → (Maybe Foreign → Boolean)
  → (∀ h. Raven h ctx → Eff (avar ∷ AVAR, raven ∷ RAVEN h | eff) a)
  → Aff (avar ∷ AVAR | eff) Boolean
requestOutputTest dsn opts ctx timeout validateResult validateSentryRequest action = do
  sentryRequestVar ← makeEmptyVar
  ret ← liftEff $ withRaven dsn
                  (insert (SProxy ∷ SProxy "dataCallback")
                          (updateWithGeneratedRequest sentryRequestVar)
                          opts)
                  ctx
                  action

  if not (validateResult ret) then pure false
    else delay timeout *> (validateSentryRequest <$> tryTakeVar sentryRequestVar)

  where
    updateWithGeneratedRequest avar = (mkEffFn1 (\x → launchAff_ (putVar x avar) *> traceAnyA x *> pure x))
