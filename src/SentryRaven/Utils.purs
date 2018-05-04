module Sentry.Raven.Utils where

import Control.Bind ((>>=))
import Control.Category ((>>>))
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign (Foreign, F)
import Data.Foreign.Index (readIndex, readProp)
import Data.Function (const)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Sentry.Raven.Core.Internal (RAVEN, Raven)
import Simple.JSON (class ReadForeign, read')

-- | Case analysis for the 'Boolean' type
bool ∷ ∀ a. a → a → Boolean → a
bool a b c = if c then b else a


-- | Useful alias for explicit type annotations.
type RavenFun0 eff ctx o = ∀ h. Raven h ctx → Eff (raven ∷ RAVEN h | eff) o

-- | Useful alias for explicit type annotations.
type RavenFun1 eff ctx i o = ∀ h. Raven h ctx → i → Eff (raven ∷ RAVEN h | eff) o

-- | Useful alias for explicit type annotations.
type RavenFun2 eff ctx i0 i1 o = ∀ h. Raven h ctx → i0 → i1 → Eff (raven ∷ RAVEN h | eff) o

-- | Represents path in a foreign object
data RIx = IxP String | IxI Int

-- | Reads foreign object subobject at given path
readSub ∷ RIx → Foreign → F Foreign
readSub = case _ of
  IxP str → readProp str
  IxI i → readIndex i

-- | Allows for convenient parsing of nested subobjects
parseForeignNested ∷ ∀ a. ReadForeign a ⇒ List RIx → Foreign → F a
parseForeignNested Nil frgn = read' frgn
parseForeignNested (Cons p ps) frgn = readSub p frgn >>= parseForeignNested ps

-- | Allows for convenient parsing of nested subobjects. Discards errors.
parseForeignNested' ∷ ∀ a. ReadForeign a ⇒ List RIx → Foreign → Maybe a
parseForeignNested' xs = parseForeignNested xs >>> runExcept >>> either (const Nothing) Just
