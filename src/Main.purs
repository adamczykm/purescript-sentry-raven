module Main where

import Prelude

import Control.Monad.Aff (Aff, delay, launchAff_)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, tryTakeVar)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1, EffFn2, runEffFn2, EffFn3, runEffFn3, EffFn4, runEffFn4, EffFn1, runEffFn1)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, F)
import Data.Foreign.Index (readProp, readIndex)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.List (List(..), (:))
import Data.Maybe (maybe, Maybe(..))
import Data.Newtype
import Data.Record (insert)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (traceAnyA)
import Simple.JSON (class ReadForeign, class WriteForeign, write, writeImpl, read)
import Type.Row (class RowLacks)

-- import Node.Process (PROCESS, lookupEnv)
import Breadcrumb

------------------------ FOREIGN ---------------------------

foreign import data Raven ∷ Type → Type → Type
foreign import data RAVEN ∷ Type → Effect


newtype Dsn = Dsn String

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
           ctx
           Unit

foreign import getContextImpl ∷
  ∀ h ctx eff
  . EffFn1 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           ctx

foreign import throw ∷ ∀ eff. Eff eff Int


------------------------ API  ---------------------------

withRaven ∷ ∀ a ctx opts eff
          . WriteForeign ctx
          ⇒ Dsn
          → opts
          → ctx
          → (∀ h. (Raven h ctx → Eff (raven ∷ RAVEN h | eff) a))
          → Eff eff a

withRaven (Dsn s) opts ctx act = runEffFn4 withRavenImpl s opts (write ctx) act

withDefRaven ∷ ∀ a ctx eff
             . WriteForeign ctx
             ⇒ Dsn
             → ctx
             → (∀ h. (Raven h ctx → Eff (raven ∷ RAVEN h | eff) a))
             → Eff eff a

withDefRaven (Dsn s) ctx act = runEffFn4 withRavenImpl s (write {}) (write ctx) act

captureException ∷ ∀ h ctx eff err
                 . WriteForeign err
                 ⇒ Raven h ctx
                 → err
                 → Eff (raven ∷ RAVEN h | eff) Unit

captureException r err = runEffFn2 captureExceptionImpl r (write err)


captureMessage ∷ ∀ h ctx eff msg
               . WriteForeign msg
               ⇒ Raven h ctx
               → msg
               → Eff (raven ∷ RAVEN h | eff) Unit

captureMessage r msg = runEffFn2 captureMessageImpl r (write msg)


getContext ∷ ∀ h ctx eff
           . Raven h ctx
           → Eff (raven ∷ RAVEN h | eff) ctx

getContext r = runEffFn1 getContextImpl r

setContext ∷ ∀ h ctx eff
           . Raven h ctx
           → ctx
           → Eff (raven ∷ RAVEN h | eff) Unit

setContext r ctx = runEffFn2 setContextImpl r ctx

modifyContext ∷ ∀ h ctx eff
              . Raven h ctx
              → (ctx → ctx)
              → Eff (raven ∷ RAVEN h | eff) Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

withNewContext ∷ ∀ h ctx ctx' eff a
               . WriteForeign ctx'
               ⇒ Raven h ctx
               → ctx'
               → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
               → Eff (raven ∷ RAVEN h | eff) a
withNewContext r ctx = withChangedContext r (const ctx)


withChangedContext ∷ ∀ h ctx ctx' eff a
                   . WriteForeign ctx'
                   ⇒ Raven h ctx
                   → (ctx → ctx')
                   → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
                   → Eff (raven ∷ RAVEN h | eff) a

withChangedContext r f action = do
  orig <- getContext r
  ret <- runEffFn3 withNewCtxImpl r (f orig) action
  setContext r orig
  pure ret



recordBreadcrumb ∷ ∀ h ctx eff r a
                 . WriteForeign a
                 ⇒ Raven h ctx
                 → Breadcrumb a
                 → Eff (raven ∷ RAVEN h | eff) Unit
recordBreadcrumb r bc = runEffFn2 recordBreadcrumbImpl r (write bc)



--------------- UTILS -------------------

-- maybe we could clean the lengthy types with these aliases?
type RavenFun0 eff o = ∀ h ctx. Raven h ctx → Eff (raven ∷ RAVEN h | eff) o
type RavenFun1 eff i o = ∀ h ctx. Raven h ctx → i → Eff (raven ∷ RAVEN h | eff) o
type RavenFun2 eff i0 i1 o = ∀ h ctx. Raven h ctx → i0 → i1 → Eff (raven ∷ RAVEN h | eff) o

data RIx = IxP String | IxI Int
readSub :: RIx -> Foreign -> F Foreign
readSub = case _ of
  IxP str → readProp str
  IxI i → readIndex i

parseForeignNested ∷ ∀ a. ReadForeign a ⇒ List RIx → Foreign → F a
parseForeignNested Nil frgn = read frgn
parseForeignNested (Cons p ps) frgn = readSub p frgn >>= parseForeignNested ps

parseForeignNested' ∷ ∀ a. ReadForeign a ⇒ List RIx → Foreign → Maybe a
parseForeignNested' xs = parseForeignNested xs >>> runExcept >>> either (const Nothing) Just


--------------- TESTS ---------------------

requestOutputTest ∷ ∀ opts eff ctx a
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
    updateWithGeneratedRequest avar = (mkEffFn1 (\x → launchAff_ (putVar x avar *> traceAnyA x *> pure x)))



---------------------- EXAMPLE & MAIN --------------------------------

data Category = Test
              | Auth
              | UI
derive instance categoryEqImpl ∷ Eq Category

instance writeForeignCategoryInst ∷ WriteForeign Category where
  writeImpl Test = writeImpl "Test"
  writeImpl Auth = writeImpl "Auth"
  writeImpl UI = writeImpl "UI"


main ∷ ∀ e. Eff (avar ∷ AVAR, console ∷ CONSOLE | e) Unit
main = launchAff_ do

  -- -------------- test
  let brdcrmb = breadcrumb Test ( _ {message = d "st brdcrmb", type = d Http, level = d Info} )
  _ ← printTestSimple "test1" $ test {} (maybe true (const false)) (\rt → do
        recordBreadcrumb rt brdcrmb
        -- captureMessage rt "st message2"
                                                                   )
  let validateBreadCrumbs mfrn = maybe false (_ == "st brdcrmb") (mfrn >>= parseForeignNested' (IxP "breadcrumbs" : IxI 0 : IxP "message" : Nil))
      validateMessageName mfrn = maybe false (_ == "st message2") (mfrn >>= parseForeignNested' (IxP "message" : Nil))

  _ ← printTestSimple "test2" $ test {tags: {x: 1}, user: {id: 1}} validateBreadCrumbs (\rt → do
        recordBreadcrumb rt brdcrmb
        captureMessage rt "st message2"
                                                                   )

  pure unit

  -------------------

  -- dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN2"

  -- tempAvar ← makeEmptyVar
  -- ret ← withRaven dsn {autoBreadcrumbs: true, dataCallback : mkEffFn1 (\x → putVar x tempAvar (const $ pure unit) *> pure x)}
  --                 {x: "some context", t: "part of ctx"} ( \r → do

  --   recordBreadcrumb r {category: Test, level:"debug", message:"st brdcrmb"}
  --   captureMessage r "st message2"
  --   traceContext r "ctx1"

  --   modifyContext r _{x="modified context"}
  --   traceContext r "ctx2"

  --   ret' <- withNewContext r {z:"changed type :)"} ( \r' → do
  --     recordBreadcrumb r' {category: UI, level:"debug", message:"st brdcrmb in changed"}
  --     captureMessage r' "st message in changed2"
  --     traceContext r' "changed ctx"
  --     pure 10

  --                                               )
  --   traceContext r "ctx2'"

  --   pure 10

  --                                                                )
  -- request ← tryTakeVar tempAvar
  -- traceAnyA request
  -- -- log (show ret)
  -- pure unit

  where
    d ∷ ∀ a. a → NullOrUndefined a
    d a = NullOrUndefined (Just a)

    printTestSimple name res = map (bool ("Test " <> name <> " has failed!") ("Test " <> name <> " Ok!")) res >>= (liftEff <<< log)

    test ∷ ∀ eff ctx a
         . WriteForeign ctx
         ⇒ ctx
         → (Maybe Foreign → Boolean)
         → (∀ h. Raven h ctx → Eff (avar ∷ AVAR, raven ∷ RAVEN h | eff) a)
         → Aff (avar ∷ AVAR | eff) Boolean
    test ctx validate action = requestOutputTest (Dsn "") {} ctx (Milliseconds 500.0) (const true) validate action

    traceContext ∷ ∀ eff. RavenFun1 (console ∷ CONSOLE | eff) String Unit
    traceContext r name = (do
      ctx <- getContext r
      traceAnyA name
      traceAnyA ctx)

    -- | Case analysis for the `Boolean` type
    bool :: forall a. a -> a -> Boolean -> a
    bool a b c = if c then b else a
