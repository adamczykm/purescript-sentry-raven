module Sentry.Raven.Breadcrumb where

import Control.Category ((>>>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (runEffFn2)
import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.Foreign (ForeignError(ForeignError), readString)
import Data.Function (const, ($))
import Data.Functor (map)
import Data.HeytingAlgebra ((&&))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord, Ordering(..))
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Unit (Unit)
import Sentry.Raven.Core.Internal (RAVEN, Raven, recordBreadcrumbImpl)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write, writeImpl)


-- | Represents possible levels of a breadcrumb (specified by Sentry API)
data Level = Critical | Error | Warning | Info | Debug

instance eqLevel ∷ Eq Level where
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
    Right "critical" → except (Right Critical)
    Right "error" → except (Right Error)
    Right "warning" → except (Right Warning)
    Right "info" → except (Right Info)
    Right "debug" → except (Right Debug)
    Right x → except (Left (singleton (ForeignError $ "Can't parse value of type Level from " <> x)))
    Left e → except (Left e)


-- | Represents possible types of a breadcrumb with special treatment from Sentry API
data Type = Default | Http | Navigation

instance eqType ∷ Eq Type where
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
    Right "default" → except (Right Default)
    Right "http" → except (Right Http)
    Right "navigation" → except (Right Navigation)
    Right x → except (Left (singleton (ForeignError $ "Can't parse value of type Type from " <> x)))
    Left e → except (Left e)


-- | Restricted record for aggregating breadcrumb data supported by Sentry API
type BreadcrumbT a d = {
  message ∷ Maybe String,
  category ∷ a,
  type ∷ Maybe Type,
  level ∷ Maybe Level,
  data ∷ Maybe d}

-- | Breadcrumb type aggregating breadcrumb data supported by Sentry API
newtype Breadcrumb a d = Breadcrumb (BreadcrumbT a d)

-- | Newtype represeting lack of data and providing necessary instances.
data X = X

instance eqX ∷ Eq X where
  eq x = const true

instance ordX ∷ Ord X where
  compare _  = const EQ

instance showX ∷ Show X where
  show = const "X"

instance wfX ∷ WriteForeign X where
  writeImpl X = writeImpl (Nothing ∷ Maybe Int)

instance rfX ∷ ReadForeign X where
  readImpl = const $ except (Right X)


-- | Breadcrumb type with no associated data
type Breadcrumb' a = Breadcrumb a X

instance eqBreadcrumbImpl ∷ (Eq a, Eq d) ⇒ Eq (Breadcrumb a d) where
  eq (Breadcrumb a) (Breadcrumb b) =
    a.category == b.category &&
    a.message == b.message &&
    a.type == b.type &&
    a.data == b.data &&
    a.level == b.level

instance rfBreadcrumb ∷ (ReadForeign a, ReadForeign d) ⇒ ReadForeign (Breadcrumb a d) where
   readImpl = readImpl >>> runExcept >>> map Breadcrumb >>> except

instance wfBreadcrumb ∷ (WriteForeign a, WriteForeign d) ⇒ WriteForeign (Breadcrumb a d) where
   writeImpl (Breadcrumb b) = writeImpl b


{- | Allows for convenient creation of a restricted breadcrumb.
     Example:

@
  'breadcrumb' "cat" ( _ {message = d "msg", type = d Navigation, level = d Info})
@
-}
breadcrumb ∷ ∀ a d. a → (BreadcrumbT a d → BreadcrumbT a d) → Breadcrumb a d
breadcrumb cat mod = Breadcrumb $ mod {
  message : Nothing,
  category : cat,
  type : Nothing,
  level : Nothing,
  data : Nothing}

{- | Allows for convenient creation of a restricted breadcrumb without carrying additional data.
     Example:

@
  'breadcrumb' "cat" ( _ {message = d "msg", type = d Navigation, level = d Info})
@
-}
breadcrumb' ∷ ∀ a. a → (BreadcrumbT a X → BreadcrumbT a X) → Breadcrumb a X
breadcrumb' cat mod = Breadcrumb $ mod {
  message : Nothing,
  category : cat,
  type : Nothing,
  level : Nothing,
  data : Nothing}

-- | Adds a breadcrumb to the current context.
-- | Notice that replacing context will cause recorded breadcrumbs to be dropped.
-- | You may also want to use 'recordBreadcrumb' from 'Sentry.Raven.Core' for
-- | non-restricted version of this function.
recordBreadcrumb' ∷
  ∀ h ctx eff a d
  . WriteForeign a
  ⇒ WriteForeign d
  ⇒ Raven h ctx
  → Breadcrumb a d
  → Eff (raven ∷ RAVEN h | eff) Unit
recordBreadcrumb' r bc = runEffFn2 recordBreadcrumbImpl r (write bc)
