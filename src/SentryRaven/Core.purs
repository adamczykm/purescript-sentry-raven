module Sentry.Raven.Core
  -- Internal reexport
  ( module Sentry.Raven.Core.Internal
  -- Raven context & lib initialization
  , Dsn(..), withRaven
  -- Event recording
  , captureException, captureMessage, recordBreadcrumb
  -- Context accessing
  , getContext
  -- Context type changing
  , withNewContext, withChangedContext, withAddedTags, withAddedExtraContext, withUser
  -- Context modification (no type changing)
  , setContext, modifyContext, setTags, modifyTags, setUser, modifyUser, setExtraContext, modifyExtraContext
  ) where


import Prim.Row (class Union, class Nub)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Data.Either (fromRight)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Record.Builder (build, merge)
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Sentry.Raven.Core.Internal (Raven)
import Sentry.Raven.Core.Internal as I
import Simple.JSON (class ReadForeign, read, class WriteForeign, write)


-- | Newtype representing Sentry's Data Source Name
newtype Dsn = Dsn String

-- | Initializes Raven library and runs given effect in given Raven context,
-- | which allows to use the rest of the library function and automatically
-- | log uncaught exceptions.
-- | For more information about rejected promises, DSN, configuration options
-- | and contexts, please consult Raven library or Sentry documentation.
withRaven ∷
  ∀ a ctx opts
  . WriteForeign ctx
  ⇒ Dsn
  → opts
  → ctx
  → (∀ h. (Raven h ctx → Effect a))
  → Effect a
withRaven (Dsn s) opts ctx act = runEffectFn4 I.withRavenImpl s opts (write ctx) act

-- | Logs exceptional behaviour of a program to given Raven instance.
-- | Parses err argument to message and stores extra as additional error data.
captureException ∷
  ∀ h ctx err extra
  . WriteForeign err
  ⇒ WriteForeign extra
  ⇒ Raven h ctx
  → err
  → extra
  → Effect Unit
captureException r err extra = runEffectFn3 I.captureExceptionImpl r (write err) (write extra)

-- | Logs non-exceptional behaviour of a program to given Raven instance.
-- | Parses msg argument as message and stores extra as additional data.
captureMessage ∷
  ∀ h ctx msg extra
  . WriteForeign msg
  ⇒ WriteForeign extra
  ⇒ Raven h ctx
  → msg
  → extra
  → Effect Unit
captureMessage r msg extra = runEffectFn3 I.captureMessageImpl r (write msg) (write extra)

-- | Adds a breadcrumb to the current context.
-- | Notice that replacing context will cause recorded breadcrumbs to be dropped.
-- | You may also want to use 'recordBreadcrumb'' from 'Sentry.Raven.Breadcrumb'
-- | for type-restricted version of this function.
recordBreadcrumb ∷ ∀ h ctx r a
                 . WriteForeign {category ∷ a | r}
                 ⇒ Raven h ctx
                 → {category ∷ a | r}
                 → Effect Unit
recordBreadcrumb r bc = runEffectFn2 I.recordBreadcrumbImpl r (write bc)

-- | Returns current Raven context.
getContext ∷
  ∀ h ctx
  . ReadForeign ctx
  ⇒ Raven h ctx
  → Effect ctx
getContext r = do
  ctx ← runEffectFn1 I.getContextImpl r
  pure $ unsafePartial fromRight (read ctx)


-- | Sets current Raven context. Notice that if you want to change the type
-- | of the context you should use 'withNewContext' function.
setContext ∷
  ∀ h ctx
  . WriteForeign ctx
  ⇒ Raven h ctx
  → ctx
  → Effect Unit
setContext r ctx = runEffectFn2 I.setContextImpl r (write ctx)

-- | Modifies current Raven context. Notice that if you want to change the type
-- | of the context you should use 'withChangedContext' function.
modifyContext ∷
  ∀ h ctx
  . WriteForeign ctx
  ⇒ ReadForeign ctx
  ⇒ Raven h ctx
  → (ctx → ctx)
  → Effect Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

-- | Runs given effect within a scope of new context.
-- | May change the type of the context.
withNewContext ∷ ∀ h ctx ctx' a
               . WriteForeign ctx ⇒ WriteForeign ctx'
               ⇒ ReadForeign ctx ⇒ ReadForeign ctx'
               ⇒ Raven h ctx
               → ctx'
               → (∀ h'. Raven h' ctx' → Effect a)
               → Effect a
withNewContext r ctx = withChangedContext r (const ctx)

-- | Runs given effect within a scope of modified context.
-- | May change the type of the context.
withChangedContext ∷
  ∀ h ctx ctx' a
  . WriteForeign ctx ⇒ WriteForeign ctx'
  ⇒ ReadForeign ctx ⇒ ReadForeign ctx'
  ⇒ Raven h ctx
  → (ctx → ctx')
  → (∀ h'. Raven h' ctx' → Effect a)
  → Effect a
withChangedContext r f action = do
  orig ← getContext r
  ret ← runEffectFn3 I.withNewCtxImpl r (f orig) action
  setContext r orig
  pure ret

-- | Adds given set of tags to the context and runs given effect within
-- | a scope of it.
-- | May change the type of the context.
withAddedTags ∷
  ∀ h ctx t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ Nub t3 t3
  ⇒ WriteForeign { tags ∷ { | t1 } | ctx}
  ⇒ ReadForeign { tags ∷ { | t1 } | ctx}
  ⇒ Raven h { tags ∷ { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { tags ∷ { | t3 } | ctx} → Effect a)
  → Effect a
withAddedTags r tags action = do
  orig ← getContext r
  let newCtx = orig { tags = build (merge orig.tags) tags}
  ret ← runEffectFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

-- | Runs given effect within a scope of additional extra context.
-- | May change the type of the context.
withAddedExtraContext ∷
  ∀ h ctx t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ Nub t3 t3
  ⇒ WriteForeign { extra ∷ { | t1 } | ctx}
  ⇒ ReadForeign { extra ∷ { | t1 } | ctx}
  ⇒ Raven h { extra ∷ { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { extra ∷ { | t3 } | ctx} → Effect a)
  → Effect a
withAddedExtraContext  r extra action = do
  orig ← getContext r
  let newCtx = orig { extra = build (merge orig.extra) extra}
  ret ← runEffectFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

-- | Runs given effect with new user context.
-- | May change the type of the context.
withUser ∷
  ∀ h ctx t1 t2 a
  . WriteForeign { user ∷ t1 | ctx}
  ⇒ ReadForeign { user ∷ t1 | ctx}
  ⇒ Raven h { user ∷ t1 | ctx}
  → t2
  → (∀ h'. Raven h' { user ∷ t2 | ctx} → Effect a)
  → Effect a
withUser r user action = do
  orig ← getContext r
  let newCtx = orig { user = user}
  ret ← runEffectFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

-- | Replaces set of tags in the current Raven context.
setTags ∷
  ∀ h ctx t1
  . WriteForeign { tags ∷ t1 | ctx}
  ⇒ ReadForeign { tags ∷ t1 | ctx}
  ⇒ Raven h { tags ∷ t1 | ctx}
  → t1
  → Effect Unit
setTags r tags = do
  orig ← getContext r
  setContext r (orig { tags = tags})
  pure unit

-- | Modifies set of tags in the current Raven context.
modifyTags ∷
  ∀ h ctx t1
  . WriteForeign{ tags ∷ t1 | ctx}
  ⇒ ReadForeign{ tags ∷ t1 | ctx}
  ⇒ Raven h { tags ∷ t1 | ctx}
  → (t1 → t1)
  → Effect Unit
modifyTags r f = do
  orig ← getContext r
  setContext r (orig { tags = (f orig.tags)})
  pure unit

-- | Replaces set user in the current Raven context.
setUser ∷
  ∀ h ctx t1
  . WriteForeign { user ∷ t1 | ctx}
  ⇒ ReadForeign { user ∷ t1 | ctx}
  ⇒ Raven h { user ∷ t1 | ctx}
  → t1
  → Effect Unit
setUser r user = do
  orig ← getContext r
  setContext r (orig { user = user})
  pure unit

-- | Modifies set user in the current Raven context.
modifyUser ∷
  ∀ h ctx t1
  . WriteForeign { user ∷ t1 | ctx}
  ⇒ ReadForeign { user ∷ t1 | ctx}
  ⇒ Raven h { user ∷ t1 | ctx}
  → (t1 → t1)
  → Effect Unit
modifyUser r f = do
  orig ← getContext r
  setContext r (orig { user = (f orig.user)})
  pure unit

-- | Replaces extra context data in the current Raven context.
setExtraContext ∷
  ∀ h ctx t1
  . WriteForeign { extra ∷ t1 | ctx}
  ⇒ ReadForeign { extra ∷ t1 | ctx}
  ⇒ Raven h { extra ∷ t1 | ctx}
  → t1
  → Effect Unit
setExtraContext r extra = do
  orig ← getContext r
  setContext r (orig { extra = extra})
  pure unit

-- | Modifies extra context data in the current Raven context.
modifyExtraContext ∷
  ∀ h ctx t1
  . WriteForeign { extra ∷ t1 | ctx}
  ⇒ ReadForeign { extra ∷ t1 | ctx}
  ⇒ Raven h { extra ∷ t1 | ctx}
  → (t1 → t1)
  → Effect Unit
modifyExtraContext r f = do
  orig ← getContext r
  setContext r (orig { extra = (f orig.extra)})
  pure unit
