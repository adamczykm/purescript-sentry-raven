/* global exports */
"use strict";

// module Sentry

var Raven = require('raven');

exports.ravenImpl = function(dsn) {
  return new Raven.Client(dsn);
};

exports.captureMessageImpl = function(raven, msg) {
  raven.captureMessage(msg);
};

exports.inContextImpl = function(raven, eff) {
  return raven.context(eff);
};

exports.throw = function() {
  return 8;
};
