module Sentry.Raven.Test where

import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind)
import Control.Monad.Aff (Aff, delay, launchAff_)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, tryTakeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Data.HeytingAlgebra (not)
import Data.Foreign (Foreign)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Sentry.Raven.Wrapper (Dsn, withRaven)
import Sentry.Raven.Wrapper.Internal (RAVEN, Raven)
import Simple.JSON (class WriteForeign)
import Type.Row (class RowLacks)

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
                  (insert (SProxy :: SProxy "dataCallback")
                          (updateWithGeneratedRequest sentryRequestVar)
                          opts)
                  ctx
                  action

  if not (validateResult ret) then pure false
    else delay timeout *> (validateSentryRequest <$> tryTakeVar sentryRequestVar)

  where
    updateWithGeneratedRequest avar = (mkEffFn1 (\x → launchAff_ (putVar x avar) *> pure x))
