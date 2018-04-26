module Sentry.Raven.Wrapper
  -- Internal reexport
  ( module Sentry.Raven.Wrapper.Internal
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


import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Control.Monad.Except (runExcept)
import Data.Either (fromRight)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Function (const, ($))
import Data.Functor ((<$>))
import Data.Record.Builder (build, merge)
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Sentry.Raven.Wrapper.Internal (Raven, RAVEN)
import Sentry.Raven.Wrapper.Internal as I
import Simple.JSON (class ReadForeign, read, class WriteForeign, write)


newtype Dsn = Dsn String

type Context user tags extra =
  { user :: NullOrUndefined user
  , tags :: NullOrUndefined tags
  , extra :: NullOrUndefined extra }

withRaven ∷
  ∀ a ctx opts eff
  . WriteForeign ctx
  ⇒ Dsn
  → opts
  → ctx
  → (∀ h. (Raven h ctx → Eff (raven ∷ RAVEN h | eff) a))
  → Eff eff a
withRaven (Dsn s) opts ctx act = runEffFn4 I.withRavenImpl s opts (write ctx) act

captureException ∷
  ∀ h ctx eff err
  . WriteForeign err
  ⇒ Raven h ctx
  → err
  → Eff (raven ∷ RAVEN h | eff) Unit
captureException r err = runEffFn2 I.captureExceptionImpl r (write err)

captureMessage ∷
  ∀ h ctx eff msg
  . WriteForeign msg
  ⇒ Raven h ctx
  → msg
  → Eff (raven ∷ RAVEN h | eff) Unit
captureMessage r msg = runEffFn2 I.captureMessageImpl r (write msg)

getContext ∷
  ∀ h ctx eff
  . ReadForeign ctx
  ⇒ Raven h ctx
  → Eff (raven ∷ RAVEN h | eff) ctx
getContext r = do
  ctx <- runEffFn1 I.getContextImpl r
  pure $ unsafePartial fromRight (runExcept (read ctx))

setContext ∷
  ∀ h ctx eff
  . WriteForeign ctx
  ⇒ Raven h ctx
  → ctx
  → Eff (raven ∷ RAVEN h | eff) Unit
setContext r ctx = runEffFn2 I.setContextImpl r (write ctx)

modifyContext ∷ ∀ h ctx eff
              . WriteForeign ctx
              ⇒ ReadForeign ctx
              ⇒ Raven h ctx
              → (ctx → ctx)
              → Eff (raven ∷ RAVEN h | eff) Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

withNewContext ∷ ∀ h ctx ctx' eff a
               . WriteForeign ctx ⇒ WriteForeign ctx'
               ⇒ ReadForeign ctx ⇒ ReadForeign ctx'
               ⇒ Raven h ctx
               → ctx'
               → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
               → Eff (raven ∷ RAVEN h | eff) a
withNewContext r ctx = withChangedContext r (const ctx)

withChangedContext ∷
  ∀ h ctx ctx' eff a
  . WriteForeign ctx ⇒ WriteForeign ctx'
  ⇒ ReadForeign ctx ⇒ ReadForeign ctx'
  ⇒ Raven h ctx
  → (ctx → ctx')
  → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
  → Eff (raven ∷ RAVEN h | eff) a
withChangedContext r f action = do
  orig <- getContext r
  ret <- runEffFn3 I.withNewCtxImpl r (f orig) action
  setContext r orig
  pure ret

withAddedTags ::
  ∀ h ctx eff t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ WriteForeign { tags :: { | t1 } | ctx}
  ⇒ ReadForeign { tags :: { | t1 } | ctx}
  ⇒ Raven h { tags :: { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { tags :: { | t3 } | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a
withAddedTags r tags action = do
  orig <- getContext r
  let newCtx = orig { tags = build (merge orig.tags) tags}
  ret <- runEffFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

withAddedExtraContext ::
  ∀ h ctx eff t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ WriteForeign { extra :: { | t1 } | ctx}
  ⇒ ReadForeign { extra :: { | t1 } | ctx}
  ⇒ Raven h { extra :: { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { extra :: { | t3 } | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a
withAddedExtraContext  r extra action = do
  orig <- getContext r
  let newCtx = orig { extra = build (merge orig.extra) extra}
  ret <- runEffFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

withUser ::
  ∀ h ctx eff t1 t2 a
  . WriteForeign { user :: t1 | ctx}
  ⇒ ReadForeign { user :: t1 | ctx}
  ⇒ Raven h { user :: t1 | ctx}
  → t2
  → (∀ h'. Raven h' { user :: t2 | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a
withUser r user action = do
  orig <- getContext r
  let newCtx = orig { user = user}
  ret <- runEffFn3 I.withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

setTags ::
  ∀ h ctx eff t1
  . WriteForeign { tags :: t1 | ctx}
  ⇒ ReadForeign { tags :: t1 | ctx}
  ⇒ Raven h { tags :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setTags r tags = do
  orig <- getContext r
  setContext r (orig { tags = tags})
  pure unit

modifyTags ::
  ∀ h ctx eff t1
  . WriteForeign{ tags :: t1 | ctx}
  ⇒ ReadForeign{ tags :: t1 | ctx}
  ⇒ Raven h { tags :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyTags r f = do
  orig <- getContext r
  setContext r (orig { tags = (f orig.tags)})
  pure unit

setUser ::
  ∀ h ctx eff t1
  . WriteForeign { user :: t1 | ctx}
  ⇒ ReadForeign { user :: t1 | ctx}
  ⇒ Raven h { user :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setUser r user = do
  orig <- getContext r
  setContext r (orig { user = user})
  pure unit

modifyUser ::
  ∀ h ctx eff t1
  . WriteForeign { user :: t1 | ctx}
  ⇒ ReadForeign { user :: t1 | ctx}
  ⇒ Raven h { user :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyUser r f = do
  orig <- getContext r
  setContext r (orig { user = (f orig.user)})
  pure unit

setExtraContext ::
  ∀ h ctx eff t1
  . WriteForeign { extra :: t1 | ctx}
  ⇒ ReadForeign { extra :: t1 | ctx}
  ⇒ Raven h { extra :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setExtraContext r extra = do
  orig <- getContext r
  setContext r (orig { extra = extra})
  pure unit

modifyExtraContext ::
  ∀ h ctx eff t1
  . WriteForeign { extra :: t1 | ctx}
  ⇒ ReadForeign { extra :: t1 | ctx}
  ⇒ Raven h { extra :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyExtraContext r f = do
  orig <- getContext r
  setContext r (orig { extra = (f orig.extra)})
  pure unit

recordBreadcrumb ∷ ∀ h ctx eff r a
                 . WriteForeign {category :: a | r}
                 ⇒ Raven h ctx
                 → {category :: a | r}
                 → Eff (raven ∷ RAVEN h | eff) Unit
recordBreadcrumb r bc = runEffFn2 I.recordBreadcrumbImpl r (write bc)
