module Sentry.Raven.Breadcrumb where


import Control.Category ((>>>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (runEffFn2)
import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Foreign (Foreign, ForeignError(ForeignError), readString)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Function (($))
import Data.Functor (map)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import Sentry.Raven.Wrapper.Internal (RAVEN, Raven, recordBreadcrumbImpl)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write, writeImpl)


data Level = Critical | Error | Warning | Info | Debug

instance eqLevel :: Eq Level where
  eq Critical Critical = true
  eq Error Error = true
  eq Warning Warning = true
  eq Info Info = true
  eq Debug Debug = true
  eq _ _ = false

instance writeForeignelInst ∷ WriteForeign Level where
  writeImpl Critical = writeImpl "critical"
  writeImpl Error = writeImpl "error"
  writeImpl Warning = writeImpl "warning"
  writeImpl Info = writeImpl "info"
  writeImpl Debug = writeImpl "debug"

instance readForeignLevelInst ∷ ReadForeign Level where
  readImpl = readString >>> runExcept >>>  case _ of
    Right "critical" -> except (Right Critical)
    Right "error" -> except (Right Error)
    Right "warning" -> except (Right Warning)
    Right "info" -> except (Right Info)
    Right "debug" -> except (Right Debug)
    Right x -> except (Left (singleton (ForeignError $ "Can't parse value of type Level from " <> x)))
    Left e -> except (Left e)

data Type = Default | Http | Navigation

instance eqType :: Eq Type where
  eq Default Default = true
  eq Http Http = true
  eq Navigation Navigation = true
  eq _ _ = false

instance writeForeignTypeInst ∷ WriteForeign Type where
  writeImpl Default = writeImpl "default"
  writeImpl Http = writeImpl "http"
  writeImpl Navigation = writeImpl "navigation"

instance readForeignTypeInst ∷ ReadForeign Type where
  readImpl = readString >>> runExcept >>>  case _ of
    Right "default" -> except (Right Default)
    Right "http" -> except (Right Http)
    Right "navigation" -> except (Right Navigation)
    Right x -> except (Left (singleton (ForeignError $ "Can't parse value of type Type from " <> x)))
    Left e -> except (Left e)

type BreadcrumbT a = {
  message :: NullOrUndefined String,
  category :: a,
  type :: NullOrUndefined Type,
  level :: NullOrUndefined Level,
  data :: NullOrUndefined Foreign}

newtype Breadcrumb a = Breadcrumb (BreadcrumbT a)

instance rfBreadcrumb :: ReadForeign a => ReadForeign (Breadcrumb a) where
   readImpl = readImpl >>> runExcept >>> map Breadcrumb >>> except

instance wfBreadcrumb :: WriteForeign a => WriteForeign (Breadcrumb a) where
   writeImpl (Breadcrumb b) = writeImpl b

breadcrumb :: forall a. a -> (BreadcrumbT a -> BreadcrumbT a) -> Breadcrumb a
breadcrumb cat mod = Breadcrumb $ mod {
  message : NullOrUndefined Nothing,
  category : cat,
  type : NullOrUndefined Nothing,
  level : NullOrUndefined Nothing,
  data : NullOrUndefined Nothing}

recordBreadcrumb' ∷ ∀ h ctx eff a
                 . WriteForeign a
                 ⇒ Raven h ctx
                 → Breadcrumb a
                 → Eff (raven ∷ RAVEN h | eff) Unit
recordBreadcrumb' r bc = runEffFn2 recordBreadcrumbImpl r (write bc)
