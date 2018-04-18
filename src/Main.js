/* global exports */
"use strict";
// module Sentry

var Raven = require('raven');

exports.ravenImpl = function(dsn, ctx) {
  var raven = new Raven.Client(dsn);
  raven.setContext(ctx);
  return raven;
};

exports.captureMessageImpl = function(raven, msg) {
  raven.captureMessage(msg);
};

exports.getContextImpl = function(raven) {
  return raven.getContext();
};

exports.setContextImpl = function(raven, ctx) {
  raven.setContext(ctx);
};

exports.inContextImpl = function(raven, eff) {
  return raven.context(eff);
};

exports.throw = function() {
  return 8;
};
