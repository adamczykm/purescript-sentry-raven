module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2, EffFn1, runEffFn1)
import Debug.Trace (traceAnyA)
import Data.Foreign (Foreign)
import Simple.JSON (class WriteForeign, write)
-- import Unsafe.Coerce (unsafeCoerce)
foreign import data Raven ∷ Type → Type
foreign import data RAVEN ∷ Effect

newtype Dsn = Dsn String

foreign import ravenImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) String ctx (Raven ctx)

foreign import captureMessageImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) String Unit

foreign import inContextImpl ∷ ∀ a ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) (Eff (raven ∷ RAVEN | eff) a) a

foreign import getContextImpl ∷ ∀ ctx eff. EffFn1 (raven ∷ RAVEN | eff) (Raven ctx) ctx

foreign import setContextImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) ctx Unit

foreign import recordBreadcrumbImpl ∷ ∀ ctx eff. EffFn2 (raven ∷ RAVEN | eff) (Raven ctx) Foreign Unit

raven ∷ ∀ ctx eff. Dsn → ctx → Eff (raven ∷ RAVEN | eff) (Raven ctx)
raven (Dsn s) ctx = runEffFn2 ravenImpl s ctx

captureMessage ∷ ∀ ctx eff. Raven ctx → String → Eff (raven ∷ RAVEN | eff) Unit
captureMessage r msg = runEffFn2 captureMessageImpl r msg

getContext ∷ ∀ ctx eff. Raven ctx → Eff (raven ∷ RAVEN | eff) ctx
getContext r = runEffFn1 getContextImpl r

setContext ∷ ∀ ctx eff. Raven ctx → ctx → Eff (raven ∷ RAVEN | eff) Unit
setContext r ctx = runEffFn2 setContextImpl r ctx

modifyContext ∷ ∀ ctx eff. Raven ctx → (ctx → ctx) → Eff (raven ∷ RAVEN | eff) Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

withContext ∷ ∀ ctx eff a. Raven ctx → ctx → (Eff (raven ∷ RAVEN | eff) a) → Eff (raven ∷ RAVEN | eff) a
withContext r ctx = withModifiedContext r (const ctx)

withModifiedContext :: ∀ ctx eff a. Raven ctx → (ctx → ctx) → (Eff (raven ∷ RAVEN | eff) a) → Eff (raven ∷ RAVEN | eff) a
withModifiedContext r f action = do
  orig <- getContext r
  setContext r (f orig)
  ret <- action
  setContext r orig
  pure ret


recordBreadcrumb :: ∀ ctx eff bc. WriteForeign bc =>  Raven ctx → bc → Eff (raven ∷ RAVEN | eff) Unit
recordBreadcrumb r bc = runEffFn2 recordBreadcrumbImpl r (write bc)




-- inContext ∷ ∀ a eff. Raven → Eff (raven ∷ RAVEN | eff) a → Eff (raven ∷ RAVEN | eff) a
-- inContext r eff = runEffFn2 inContextImpl r eff

foreign import throw ∷ ∀ eff. Eff eff Int

main ∷ forall e. Eff (console ∷ CONSOLE, raven ∷ RAVEN | e) Unit
main = do
  r ← raven (Dsn "") -- put dsn here
            {x: "Some context"}

  context ← getContext r

  setContext r {x: "New context"}

  traceAnyA "Old context:"
  traceAnyA context

  withModifiedContext r _{x="Inner"} (do
    inner ← getContext r
    traceAnyA "inner context:"
    traceAnyA inner)

  newContext ← getContext r
  traceAnyA "New context:"
  traceAnyA newContext

  traceAnyA "Old context:"
  traceAnyA context
  recordBreadcrumb r {level: "error", category: "test", message: "test message"}
  captureMessage r ("MESSAGE catch this 3")

  recordBreadcrumb r {level: "warning", category: "test", message: "warning brd"}
  -- _ ← throw -- uncommenting this line will cause last messages not to be send
  --           -- as the exception is not hadnled by raven

  recordBreadcrumb r {level: "warning", category: "test", message: "warning brd"}

  captureMessage r ("MESSAGE catch this 4")
  -- -- x ← y r

  -- x ← inContext r throw
  -- logShow x
  -- traceAnyA x
  -- captureMessage r "NO GLOB"
  -- traceAnyA r

  -- traceAnyA r
