module Example.SimpleUsage where

import Prelude (class Eq, Unit, bind, const, discard, identity, map, pure, unit, ($), (<$>), (<<<), (<>), (==), (>>=))

import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign)
import Data.List (List(..), (:))
import Data.Maybe (maybe, Maybe(..))
import Data.Time.Duration (Milliseconds(..))
-- import Debug.Trace (traceAnyA)
-- import Node.Process (lookupEnv)
import Simple.JSON (class ReadForeign, class WriteForeign, write, writeImpl)

import Sentry.Raven (Dsn(..), RIx(..), Raven, RavenFun1, captureMessage, getContext, parseForeignNested', withAddedExtraContext, withAddedTags, withRaven, withUser)
import Sentry.Raven.Breadcrumb
-- import Sentry.Raven.Test (requestOutputTest)


---------------------- EXAMPLE & MAIN --------------------------------

data Category = Test
              | Auth
              | UI
derive instance categoryEqImpl ∷ Eq Category

instance writeForeignCategoryInst ∷ WriteForeign Category where
  writeImpl Test = writeImpl "Test"
  writeImpl Auth = writeImpl "Auth"
  writeImpl UI = writeImpl "UI"


main ∷ ∀ e. Effect Unit
main = launchAff_ do

  -- -------------- test

  -- let brdcrmb = breadcrumb Test ( _ {message = d "st brdcrmb", type = d Http, level = d Info, data=d (write {url: "http://example.com/api/1.0/users", method:"GET", status_code:200, reason:"OK"})} )
  let brdcrmb = breadcrumb Test ( _ {message = j "st brdcrmb", type = j Navigation, level = j Info, data=j (write {from: "/from", to:"/to"})} )
  _ ← printTestSimple "test1" $ test {} (maybe true (const false)) (\rt → do
        recordBreadcrumb' rt brdcrmb
                                                                   )
  let validateBreadCrumbs mfrn = maybe false (_ == "st brdcrmb") (mfrn >>= parseForeignNested' (IxP "breadcrumbs" : IxP "values" : IxI 0 : IxP "message" : Nil))
      validateMessageName mfrn = maybe false (_ == "st message2") (mfrn >>= parseForeignNested' (IxP "message" : Nil))

  _ ← printTestSimple "test2" $ test {extra: {contexts: {os: {name: "GNU/Linux"}}}, tags: {x: 1}, user: {id: 1}} validateBreadCrumbs (\rt → do
        recordBreadcrumb' rt brdcrmb
        captureMessage rt "test message 20" {}
        ctx ← getContext rt
        traceAnyA ctx )

  _ ← liftEffect (do
      dsn ← (Dsn <<< maybe "" identity) <$> lookupEnv "SENTRY_DSN"

      ret ← withRaven dsn {} { user : {id : 1}, tags : {}, extra : {a:1} } ( \r'' →
        withAddedTags r'' {tag1 : 1, tag2 : "2"} (\r' → do
          withUser r' {id: 1, email: "pure@script.org"} (\r3 → do
            withAddedExtraContext r3 {b:2} (\r → do
              recordBreadcrumb' r brdcrmb
              captureMessage r "st message2" {}
              ctx ← getContext r
              traceAnyA ctx
            ))))

      pure unit
               )

  pure unit

  where
    j ∷ ∀ a. a → Maybe a
    j = Just

    printTestSimple name res = map (bool ("Test " <> name <> " has failed!") ("Test " <> name <> " Ok!")) res >>= (liftEffect <<< log)

    test ∷ ∀ ctx a
         . WriteForeign ctx
         ⇒ ReadForeign ctx
         ⇒ ctx
         → (Maybe Foreign → Boolean)
         → (∀ h. Raven h ctx → Effect a)
         → Aff Boolean
    test ctx validate action = requestOutputTest (Dsn "") {} ctx (Milliseconds 1500.0) (const true) validate action

    traceContext ∷ ∀ ctx eff. ReadForeign ctx ⇒ RavenFun1 ctx String Unit
    traceContext r name = (do
      ctx ← getContext r
      traceAnyA name
      traceAnyA ctx)

    -- | Case analysis for the 'Boolean' type
    bool ∷ forall a. a → a → Boolean → a
    bool a b c = if c then b else a
