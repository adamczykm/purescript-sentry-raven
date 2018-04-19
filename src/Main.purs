module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2, EffFn3, runEffFn3, EffFn1, runEffFn1)
import Node.Process (PROCESS, lookupEnv)
import Data.Maybe (maybe)
import Debug.Trace (traceAnyA)
import Data.Foreign (Foreign)
import Simple.JSON (class WriteForeign, write, writeImpl)

foreign import data Raven :: Type -> Type -> Type
foreign import data RAVEN :: Type -> Effect


newtype Dsn = Dsn String

foreign import withRavenImpl ::
  ∀ ctx eff a
  . EffFn3 eff
           String
           Foreign
           (∀ h. Raven h ctx -> Eff (raven :: RAVEN h | eff) a)
           a

foreign import withNewCtxImpl ::
  ∀ ctx ctx' eff a h
  . EffFn3 (raven :: RAVEN h | eff)
           (Raven h ctx)
           ctx'
           (∀ h'. Raven h' ctx' -> Eff (raven :: RAVEN h' | eff) a)
           a

foreign import captureMessageImpl :: ∀ h ctx eff. EffFn2 (raven :: RAVEN h | eff) (Raven h ctx) Foreign Unit

foreign import recordBreadcrumbImpl :: ∀ h ctx eff. EffFn2 (raven :: RAVEN h | eff) (Raven h ctx) Foreign Unit

foreign import setContextImpl :: ∀ h ctx eff. EffFn2 (raven :: RAVEN h | eff) (Raven h ctx) ctx Unit

foreign import getContextImpl :: ∀ h ctx eff. EffFn1 (raven :: RAVEN h | eff) (Raven h ctx) ctx

foreign import throw :: ∀ eff. Eff eff Int


withRaven :: ∀ a ctx eff. WriteForeign ctx
          => Dsn
          -> ctx
          -> (∀ h. (Raven h ctx -> Eff (raven :: RAVEN h | eff) a))
          -> Eff eff a

withRaven (Dsn s) ctx act = runEffFn3 withRavenImpl s (write ctx) act

captureMessage :: ∀ h ctx eff msg. WriteForeign msg
               => Raven h ctx
               -> msg
               -> Eff (raven :: RAVEN h | eff) Unit

captureMessage r msg = runEffFn2 captureMessageImpl r (write msg)

getContext :: ∀ h ctx eff
            . Raven h ctx
           -> Eff (raven :: RAVEN h | eff) ctx

getContext r = runEffFn1 getContextImpl r

setContext :: ∀ h ctx eff
            . Raven h ctx
           -> ctx
           -> Eff (raven :: RAVEN h | eff) Unit

setContext r ctx = runEffFn2 setContextImpl r ctx

modifyContext :: ∀ h ctx eff
               . Raven h ctx
              -> (ctx -> ctx)
              -> Eff (raven :: RAVEN h | eff) Unit
modifyContext r f = (f <$> getContext r) >>= setContext r

withNewContext :: ∀ h ctx ctx' eff a. WriteForeign ctx'
               => Raven h ctx
               -> ctx'
               -> (∀ h'. Raven h' ctx' -> Eff (raven :: RAVEN h' | eff) a)
               -> Eff (raven :: RAVEN h | eff) a
withNewContext r ctx = withChangedContext r (const ctx)

withChangedContext :: ∀ h ctx ctx' eff a. WriteForeign ctx'
                   => Raven h ctx
                   -> (ctx -> ctx')
                   -> (∀ h'. Raven h' ctx' -> Eff (raven :: RAVEN h' | eff) a)
                   -> Eff (raven :: RAVEN h | eff) a

withChangedContext r f action = do
  orig <- getContext r
  ret <- runEffFn3 withNewCtxImpl r (f orig) action
  setContext r orig
  pure ret



type Breadcrumb a r = { category :: a | r }

recordBreadcrumb :: ∀ h ctx eff r a. WriteForeign (Breadcrumb a r) => Raven h ctx -> Breadcrumb a r -> Eff (raven :: RAVEN h | eff) Unit
recordBreadcrumb r bc = runEffFn2 recordBreadcrumbImpl r (write bc)

data Category = Test
              | Auth
              | UI

instance writeForeignCategoryInst :: WriteForeign Category where
  writeImpl Test = writeImpl "Test"
  writeImpl Auth = writeImpl "Auth"
  writeImpl UI = writeImpl "UI"


newtype Id = Id Int
derive newtype instance rfI :: WriteForeign Id


-- derive instance categoryShow :: Show Category
-- derive instance categoryWriteForeign :: WriteForeign Category

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS | e) Unit
main = do
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"
  ret ← withRaven dsn
                  {x: "Some context", t: "part of ctx"} ( \r -> do
    recordBreadcrumb r {category: Test, level:"debug", message:"st brdcrmb"}
    captureMessage r "st message2"
    traceContext r "ctx1"

    modifyContext r _{x="modified context"}
    traceContext r "ctx2"

    ret' <- withNewContext r {z:"changed type :)"} ( \r' -> do
      recordBreadcrumb r' {category: UI, level:"debug", message:"st brdcrmb in changed"}
      captureMessage r' "st message in changed2"
      traceContext r' "changed ctx"
      pure 10
                                                )
    traceContext r "ctx2'"

    pure ret'

                                                                 )
  log (show ret)
  pure unit

  where
    traceContext :: forall h ctx eff. Raven h ctx -> String -> Eff (raven :: RAVEN h, console :: CONSOLE | eff) Unit
    traceContext r name = (do
      ctx <- getContext r
      traceAnyA name
      traceAnyA ctx
                          )
