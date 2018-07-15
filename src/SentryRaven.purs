module Sentry.Raven
  ( module Sentry.Raven.Breadcrumb
  , module Sentry.Raven.Utils
  , module Sentry.Raven.Core
  ) where


import Sentry.Raven.Breadcrumb (
  X(..), Level(..), Type(..), BreadcrumbT, Breadcrumb', Breadcrumb(..),
  breadcrumb, recordBreadcrumb')


import Sentry.Raven.Utils (
  -- common combinators
  bool,
  -- type aliases
  RavenFun0, RavenFun1, RavenFun2,
  -- nested foreign parsing
  RIx(..), readSub, parseForeignNested, parseForeignNested')


import Sentry.Raven.Core (
  -- Internal reexport
  Raven,
  -- Raven context & lib initialization
  Dsn(..), withRaven,
  -- Event recording
  captureException, captureMessage, recordBreadcrumb,
  -- Context accessing
  getContext,
  -- Context type changing
  withNewContext, withChangedContext, withAddedTags, withAddedExtraContext, withUser,
  -- Context modification (no type changing)
  setContext, modifyContext, setTags, modifyTags, setUser, modifyUser, setExtraContext, modifyExtraContext)
