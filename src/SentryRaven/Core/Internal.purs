module Sentry.Raven.Core.Internal where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Foreign (Foreign)
import Data.Unit (Unit)

foreign import data Raven ∷ Type → Type → Type

foreign import withRavenImpl ∷
  ∀ ctx cfg a
  . EffectFn4 String
      cfg
      Foreign
      (∀ h. Raven h ctx → Effect a)
      a

foreign import withNewCtxImpl ∷
  ∀ ctx ctx' a h
  . EffectFn3
      (Raven h ctx)
      ctx'
      (∀ h'. Raven h' ctx' → Effect a)
      a

foreign import captureMessageImpl ∷
  ∀ h ctx
  . EffectFn3
      (Raven h ctx)
      Foreign
      Foreign
      Unit

foreign import captureExceptionImpl ∷
  ∀ h ctx
  . EffectFn3
      (Raven h ctx)
      Foreign
      Foreign
      Unit

foreign import recordBreadcrumbImpl ∷
  ∀ h ctx
  . EffectFn2
      (Raven h ctx)
      Foreign
      Unit

foreign import setContextImpl ∷
  ∀ h ctx
  . EffectFn2
      (Raven h ctx)
      Foreign
      Unit

foreign import getContextImpl ∷
  ∀ h ctx
  . EffectFn1
      (Raven h ctx)
      Foreign
