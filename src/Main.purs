module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2, EffFn1, runEffFn1)
import Debug.Trace (traceAnyA)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Raven ∷ Type → Type
foreign import data RAVEN ∷ Effect

newtype Dsn = Dsn String

foreign import ravenImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) String ctx (Raven ctx)

foreign import captureMessageImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) String Unit

foreign import inContextImpl ∷ ∀ a ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) (Eff (raven ∷ RAVEN | eff) a) a

foreign import getContextImpl ∷ ∀ ctx eff. EffFn1 (raven ∷ RAVEN | eff) (Raven ctx) ctx

foreign import setContextImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) ctx Unit

raven ∷ ∀ ctx eff. Dsn → ctx → Eff (raven ∷ RAVEN | eff) (Raven ctx)
raven (Dsn s) ctx = runEffFn2 ravenImpl s ctx

captureMessage ∷ ∀ ctx eff. Raven ctx → String → Eff (raven ∷ RAVEN | eff) Unit
captureMessage r msg = runEffFn2 captureMessageImpl r msg

getContext ∷ ∀ ctx eff. Raven ctx → Eff (raven ∷ RAVEN | eff) ctx
getContext r = runEffFn1 getContextImpl r

setContext ∷ ∀ ctx eff. Raven ctx → ctx → Eff (raven ∷ RAVEN | eff) Unit
setContext r ctx = runEffFn2 setContextImpl r ctx

-- inContext ∷ ∀ a eff. Raven → Eff (raven ∷ RAVEN | eff) a → Eff (raven ∷ RAVEN | eff) a
-- inContext r eff = runEffFn2 inContextImpl r eff

foreign import throw ∷ ∀ eff. Eff eff Int

main :: forall e. Eff (console :: CONSOLE, raven ∷ RAVEN | e) Unit
main = do
  r ← raven (Dsn "https://179ded71931e4a58b05b892dd837852e@sentry.io/1191644") {x: "Some context"}

  context ← getContext r

  setContext r {x: "New context"}

  traceAnyA "Old context:"
  traceAnyA context

  newContext ← getContext r
  traceAnyA "New context:"
  traceAnyA newContext

  traceAnyA "Old context:"
  traceAnyA context

  captureMessage r ("TEST MESSAGE")
  -- -- x ← y r
  -- x ← inContext r throw
  -- logShow x
  -- traceAnyA x
  -- captureMessage r "NO GLOB"
  -- traceAnyA r
