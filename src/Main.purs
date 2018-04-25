module Main where

import Prelude

import Breadcrumb (Breadcrumb, Level(..), Type(..), breadcrumb)
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
import Data.Record (insert)
import Data.Record.Builder (build, merge)
import Data.Symbol (SProxy(SProxy))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (traceAnyA)
import Node.Process (PROCESS, lookupEnv)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write, writeImpl)
import Type.Row (class RowLacks)

------------------------ FOREIGN ---------------------------

foreign import data Raven ∷ Type → Type → Type
foreign import data RAVEN ∷ Type → Effect


somefunction2 ∷ Int → Int
somefunction2 = id


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
           Foreign
           Unit

foreign import getContextImpl ∷
  ∀ h ctx eff
  . EffFn1 (raven ∷ RAVEN h | eff)
           (Raven h ctx)
           ctx

foreign import throw ∷ ∀ eff. Eff eff Int


------------------------ API  ---------------------------

-- type User extra =
--   { id :: NullOrUndefined Int
--   , username :: NullOrUndefined String
--   , email :: NullOrUndefined String
--   , ip_address :: NullOrUndefined String
--   | extra
-- }

undefined :: forall a. NullOrUndefined a
undefined = NullOrUndefined Nothing


withRaven ∷ ∀ a ctx opts eff
          . WriteForeign ctx
          ⇒ Dsn
          → opts
          → ctx
          → (∀ h. (Raven h ctx → Eff (raven ∷ RAVEN h | eff) a))
          → Eff eff a

withRaven (Dsn s) opts ctx act = runEffFn4 withRavenImpl s opts (write ctx) act

type Context user tags extra = {user :: NullOrUndefined user, tags :: NullOrUndefined tags, extra :: NullOrUndefined extra}
withDefRaven ∷ ∀ a eff user tags extra
             . WriteForeign (Context user tags extra)
             ⇒ Dsn
             → (∀ h. (Raven h (Context user tags extra) → Eff (raven ∷ RAVEN h | eff) a))
             → Eff eff a

withDefRaven (Dsn s) act = runEffFn4 withRavenImpl s {} (write ({user: undefined, tags: undefined, extra:undefined} ∷ Context user tags extra)) act

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
           . WriteForeign ctx
           ⇒ Raven h ctx
           → ctx
           → Eff (raven ∷ RAVEN h | eff) Unit

setContext r ctx = runEffFn2 setContextImpl r (write ctx)

modifyContext ∷ ∀ h ctx eff
              . WriteForeign ctx
              ⇒ Raven h ctx
              → (ctx → ctx)
              → Eff (raven ∷ RAVEN h | eff) Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

withNewContext ∷ ∀ h ctx ctx' eff a
               . WriteForeign ctx ⇒ WriteForeign ctx'
               ⇒ Raven h ctx
               → ctx'
               → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
               → Eff (raven ∷ RAVEN h | eff) a
withNewContext r ctx = withChangedContext r (const ctx)


withChangedContext ∷ ∀ h ctx ctx' eff a
                   . WriteForeign ctx ⇒ WriteForeign ctx'
                   ⇒ Raven h ctx
                   → (ctx → ctx')
                   → (∀ h'. Raven h' ctx' → Eff (raven ∷ RAVEN h' | eff) a)
                   → Eff (raven ∷ RAVEN h | eff) a

withChangedContext r f action = do
  orig <- getContext r
  ret <- runEffFn3 withNewCtxImpl r (f orig) action
  setContext r orig
  pure ret



withAddedTags ::
  ∀ h ctx eff t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ WriteForeign { tags :: { | t1 } | ctx}
  ⇒ Raven h { tags :: { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { tags :: { | t3 } | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a

withAddedTags r tags action = do
  orig <- getContext r
  let newCtx = orig { tags = build (merge orig.tags) tags}
  ret <- runEffFn3 withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

-- type WithUser a = {user :: {|a}}
-- withUser ::
--   ∀ h ctx eff t1 t2 t3 a
--   . Union (WithUser t2) ctx t3
--   ⇒ Raven h { | ctx}
--   → { | t2 }
--   → (∀ h'. Raven h' { | t3 } -> Eff ( raven :: RAVEN h' | eff) a)
--   → Eff ( raven :: RAVEN h | eff) a

withAddedExtraContext ::
  ∀ h ctx eff t1 t2 t3 a
  . Union t2 t1 t3
  ⇒ WriteForeign { extra :: { | t1 } | ctx}
  ⇒ Raven h { extra :: { | t1 } | ctx}
  → { | t2 }
  → (∀ h'. Raven h' { extra :: { | t3 } | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a

withAddedExtraContext  r extra action = do
  orig <- getContext r
  let newCtx = orig { extra = build (merge orig.extra) extra}
  ret <- runEffFn3 withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

withUser ::
  ∀ h ctx eff t1 t2 t3 a
  . WriteForeign { user :: t1 | ctx} 
  ⇒ Raven h { user :: t1 | ctx}
  → t2
  → (∀ h'. Raven h' { user :: t2 | ctx} -> Eff ( raven :: RAVEN h' | eff) a)
  → Eff ( raven :: RAVEN h | eff) a
withUser r user action = do
  orig <- getContext r
  let newCtx = orig { user = user}
  ret <- runEffFn3 withNewCtxImpl r newCtx action
  setContext r orig
  pure ret

setTags ::
  ∀ h ctx eff t1 a
  . WriteForeign { tags :: t1 | ctx} 
  ⇒ Raven h { tags :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setTags r tags = do
  orig <- getContext r
  setContext r (orig { tags = tags})
  pure unit

modifyTags ::
  ∀ h ctx eff t1 a
  . WriteForeign{ tags :: t1 | ctx} 
  ⇒ Raven h { tags :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyTags r f = do
  orig <- getContext r
  setContext r (orig { tags = (f orig.tags)})
  pure unit

setUser ::
  ∀ h ctx eff t1 a
  . WriteForeign { user :: t1 | ctx}
  ⇒ Raven h { user :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setUser r user = do
  orig <- getContext r
  setContext r (orig { user = user})
  pure unit

modifyUser ::
  ∀ h ctx eff t1 a
  . WriteForeign { user :: t1 | ctx}
  ⇒ Raven h { user :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyUser r f = do
  orig <- getContext r
  setContext r (orig { user = (f orig.user)})
  pure unit

setExtraContext ::
  ∀ h ctx eff t1 t2 t3 a
  . WriteForeign { extra :: t1 | ctx}
  ⇒ Raven h { extra :: t1 | ctx}
  → t1
  → Eff ( raven :: RAVEN h | eff) Unit
setExtraContext r extra = do
  orig <- getContext r
  setContext r (orig { extra = extra})
  pure unit

modifyExtraContext ::
  ∀ h ctx eff t1 a
  . WriteForeign { extra :: t1 | ctx}
  ⇒ Raven h { extra :: t1 | ctx}
  → (t1 -> t1)
  → Eff ( raven :: RAVEN h | eff) Unit
modifyExtraContext r f = do
  orig <- getContext r
  setContext r (orig { extra = (f orig.extra)})
  pure unit


-- has to wait till 0.12 needs Nub class to avoid label duplication
-- mergeX :: forall a b xb c. RowCons "x" b RowNil xb => Union a xb c => { | b } -> { | a } -> { | c}
-- mergeX a b  = build (merge a) {x: b}

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
    updateWithGeneratedRequest avar = (mkEffFn1 (\x → launchAff_ (putVar x avar *> traceAnyA x) *> pure x))



---------------------- EXAMPLE & MAIN --------------------------------

data Category = Test
              | Auth
              | UI
derive instance categoryEqImpl ∷ Eq Category

instance writeForeignCategoryInst ∷ WriteForeign Category where
  writeImpl Test = writeImpl "Test"
  writeImpl Auth = writeImpl "Auth"
  writeImpl UI = writeImpl "UI"


main ∷ ∀ e. Eff (process :: PROCESS, avar ∷ AVAR, console ∷ CONSOLE | e) Unit
main = launchAff_ do

  -- -------------- test

  -- let brdcrmb = breadcrumb Test ( _ {message = d "st brdcrmb", type = d Http, level = d Info, data=d (write {url: "http://example.com/api/1.0/users", method:"GET", status_code:200, reason:"OK"})} )
  let brdcrmb = breadcrumb Test ( _ {message = d "st brdcrmb", type = d Navigation, level = d Info, data=d (write {from: "/from", to:"/to"})} )
  _ ← printTestSimple "test1" $ test {} (maybe true (const false)) (\rt → do
        recordBreadcrumb rt brdcrmb
        -- captureMessage rt "st message2"
                                                                   )
  let validateBreadCrumbs mfrn = maybe false (_ == "st brdcrmb") (mfrn >>= parseForeignNested' (IxP "breadcrumbs" : IxP "values" : IxI 0 : IxP "message" : Nil))
      validateMessageName mfrn = maybe false (_ == "st message2") (mfrn >>= parseForeignNested' (IxP "message" : Nil))

  _ ← printTestSimple "test2" $ test {extra: {contexts: {os: {name: "GNU/Linux"}}}, tags: {x: 1}, user: {id: 1}} validateBreadCrumbs (\rt → do
        recordBreadcrumb rt brdcrmb
        captureMessage rt "test message 20"
        ctx <- getContext rt
        traceAnyA ctx

                                                                   )


  -------------------
  _ <- liftEff (do
      dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"

      ret ← withRaven dsn {} { user : {id : 1}, tags : {}, extra : {a:1} } ( \r'' →
        withAddedTags r'' {tag1 : 1, tag2 : "2"} (\r' -> do
          withUser r' {id: 1, email: "pure@script.org"} (\r3 -> do
            withAddedExtraContext r3 {b:2} (\r -> do
              recordBreadcrumb r brdcrmb
              captureMessage r "st message2"
              ctx <- getContext r
              traceAnyA ctx
            ))))

      pure unit
               )

  _ <- liftEff (do
      let dsn = Dsn ""

      ret ← withDefRaven dsn ( \r → (do
        setTags r (d {a:1,b:"2"})
        setUser r (d {username: "anks"})
        setExtraContext r (d {some: "thing"})
        -- ctx <- getContext r
        -- traceAnyA ctx
        captureMessage r "whatevah2"
            ))

      pure unit
               )

  pure unit
    -- traceContext r "ctx1"

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
    test ctx validate action = requestOutputTest (Dsn "") {} ctx (Milliseconds 1500.0) (const true) validate action

    traceContext ∷ ∀ eff. RavenFun1 (console ∷ CONSOLE | eff) String Unit
    traceContext r name = (do
      ctx <- getContext r
      traceAnyA name
      traceAnyA ctx)

    -- | Case analysis for the `Boolean` type
    bool :: forall a. a -> a -> Boolean -> a
    bool a b c = if c then b else a



-- defUser :: forall a b c d.
--   { id :: NullOrUndefined a
--   , username :: NullOrUndefined b
--   , email :: NullOrUndefined c
--   , ip_address :: NullOrUndefined d
--   }
-- defUser = {id: undefined, username : undefined, email : undefined, ip_address : undefined}

-- type Context tags user extra = { tags :: NullOrUndefined tags, user :: NullOrUndefined (User user), extra :: NullOrUndefined extra}

-- -- get :: forall r r' l a. IsSymbol l => RowCons l a r' r => SProxy l -> { | r } -> a

-- getMb :: forall r r' l a. IsSymbol l => RowCons l (NullOrUndefined a) r' r => { | r } -> SProxy l -> Maybe a
-- getMb r x = unwrap $ get x r


-- unwrapGet :: forall r p1 p2. (r -> NullOrUndefined p1) -> (p1 -> NullOrUndefined p2) -> (r -> Maybe p2)
-- unwrapGet g1 g2 = \r -> case unwrap (g1 r) of
--   Nothing -> Nothing
--   Just rn -> unwrap (g2 rn)

-- infixl 9 unwrapGet as ?.
-- flapl = flip ($)
-- infixr 10 flapl as &

-- s = SProxy

-- defContext :: forall a b c. WriteForeign (Context a b c) => Context a b c
-- defContext = { tags: undefined, user: undefined, extra: undefined }

-- withRaven2 ∷
--   ∀ tags user extra opts eff a
--   . WriteForeign (Context tags user extra)
--   ⇒ Dsn
--   → opts
--   → (Context tags user extra -> Context tags user extra)
--   → (∀ h. (Raven h (Context tags user extra) → Eff (raven ∷ RAVEN h | eff) a))
--   → Eff eff a
-- withRaven2 (Dsn s) opts ctxMod act = runEffFn4 withRavenImpl s opts (write (ctxMod defContext)) act

-- user3 = (runExcept (read (write ({user : (NullOrUndefined Nothing)} :: {user :: (NullOrUndefined Int)})))) :: Either (NonEmptyList ForeignError) {user :: (NullOrUndefined Int)}
