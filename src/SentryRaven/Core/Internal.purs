module Sentry.Raven.Core.Internal where

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, EffFn4, EffFn1)
import Data.Foreign (Foreign)
import Data.Unit (Unit)

foreign import data Raven ∷ Type → Type → Type
foreign import data RAVEN ∷ Type → Effect

foreign import withRavenImpl ∷
  ∀ ctx eff cfg a
  . EffFn4 eff
           String
           cfg
           Foreign
           (∀ h. Raven h ctx → Eff (raven ∷ RAVEN h | eff) a)
           a

foreign import withNewCtxImpl ∷
  ∀ ctx ctx' eff a h
  . EffFn3 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           ctx'
           (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
           a

foreign import captureMessageImpl ∷
  ∀ h ctx eff
  . EffFn2 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           Foreign
           Unit

foreign import captureExceptionImpl ∷
  ∀ h ctx eff
  . EffFn2 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           Foreign
           Unit

foreign import recordBreadcrumbImpl ∷
  ∀ h ctx eff
  . EffFn2 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           Foreign
           Unit

foreign import setContextImpl ∷
  ∀ h ctx eff
  . EffFn2 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           Foreign
           Unit

foreign import getContextImpl ∷
  ∀ h ctx eff
  . EffFn1 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           Foreign
