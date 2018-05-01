# purescript-sentry-raven

Purescript client library for [`Sentry.io`](http://sentry.io) wrapping Javascript client - Raven.

# Credits

Library is co-created and funded by [`λ-terms`](https://github.com/lambdaterms/)

# Library status & important info

## Stability

Library is not mature and may be unstable. Please report any issues via Github's tracker. Additionally should you have any ideas on how to improve any aspect of the library, we're open for suggestions and PR's.

## Supported platforms

Currently the only supported platform is Node.js. For the library to work on Node you need to install Raven. Plans are to also support Raven.js client with one common API.

```
npm i raven
```

## Auto-breadcrumbs

Auto-breadcrumbs are not yet supported.

## Breadcrumb alerts with Raven-node

Raven-node will alert that breadcrumbs will not be registered as the library module install() function was not called. You can safely ignore these warnings.

# Getting Started

## Installation

TODO

## Tests

There is a small suite of unit tests accompanying the library. You can run it with:
```
pulp test
```

## Introduction

Sentry is a popular error tracking platform that helps developers monitor and fix crashes in real-time.
For more information how to use it consult its docs page: https://docs.sentry.io/
Purescript client is based on few main functions that introduce and manipulate Raven context, and capture events and breadcrumbs.

Most of the library functions are only usable within Raven scope. All uncaught exceptions within the scope will be reported to Sentry (rejected promises handling may be specific to the platform). Here's how to introduce it:

```purescript

main = do
  -- read data source name
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"
  -- introduce raven scope, with default configuration and empty context
  ret ← withRaven dsn {} {} ( \r → do
    ... -- application code
    )
              
```

### Events & breadcrumbs

Apart from automatic reporting uncaught exceptions you can manually report exceptional and unexceptional program behaviour (events) with `captureException` and `captureMessage`, and record breadcrumbs with `recordBreadcrumb` and `recordBreadcrum'`. Breadcrumbs serve role of a trail of events that happened before the event and might help identifying a problem. After recording each event is sent to Sentry along with context and breadcrumb trail (see Sentry docs) present at given time.

```purescript

main = do
  -- read data source name
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"
  -- introduce raven scope, with default configuration and empty context
  ret ← withRaven dsn {} {} ( \r → do
    ...
    recordBreadcrumb' r (breadcrumb' _{message="Cart was emptied."}_)
    ...
    captureMessage r "Unusual user behaviour" {}
  )
              
```

### Context and Raven type parameters

Sentry supports additional context with events. Often this context is shared amongst any issue captured in its lifecycle, and includes the following components:

Tags  
    Key/value pairs which generate breakdowns charts and search filters  
User  
    Information about the current actor  
Extra  
    Arbitrary unstructured data which is stored with an event sample  

Context is a part of Raven type and thus changing its type would change the type of Raven scope. This is supported with dedicated functions such as `withUser`:

```purescript

  withRaven dsn config {} ( \r →
    ... -- user 1 logged in
    withUser r {id: 1, username: user1, email: hello@lambdaterms.com} (\r' → do

      ... -- user changed username
      modifyUser r' _{username = "paluh"}


    ))
              
```

### Configuration

You can pass configuration option to the native Raven library as a second argument to the function. For accepted options consult correspoding library documentation.


```purescript

main = do
  -- read data source name
  dsn ← (Dsn <<< maybe "" id) <$> lookupEnv "SENTRY_DSN"

  -- use ad hoc configuration
  let config = {release: '1.3.0', environment: 'staging'}

  -- introduce raven scope, with default configuration and empty context
  ret ← withRaven dsn config {} ( \r →
    ... -- application code
    )
              
```


## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-sentry-raven).

## License & copyrights

See LICENSE file.
