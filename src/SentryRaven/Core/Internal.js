"use strict";

var Raven = require('raven');

exports.setContextHelper = function(raven, ctx){
    var ctx_cpy = ctx == null ? {} : JSON.parse(JSON.stringify(ctx));
    raven.setContext(ctx_cpy);
    return ctx_cpy;
};

exports.withRavenImpl = function(dsn, options, ctx, act) {
    console.log("withravenimpl: ");
    var raven = new Raven.Client(dsn, options);
    console.log("ctx: ");
    console.log(ctx_cpy);
    var ctx_cpy = exports.setContextHelper(raven,ctx);
    return raven.context(ctx_cpy, function(){
        return act(raven)();
    });
};

exports.withNewCtxImpl = function(raven, ctx, act) {
    var ctx_cpy = exports.setContextHelper(raven,ctx);
    return raven.context(ctx_cpy, function(){
        return act(raven)();
    });
};

exports.captureMessageImpl = function(raven, msg) {
    raven.captureMessage(msg);
};

exports.captureExceptionImpl = function(raven, err) {
    raven.captureException(err);
};

exports.getContextImpl = function(raven) {
  return raven.getContext();
};

exports.setContextImpl = function(raven, ctx) {
    exports.setContextHelper(raven,ctx);
};

exports.extend =
    Object.assign ||
    function(target) {
        for (var i = 1; i < arguments.length; i++) {
            var source = arguments[i];
            for (var key in source) {
                if (Object.prototype.hasOwnProperty.call(source, key)) {
                    target[key] = source[key];
                }
            }
        }
        return target;
    };

exports.recordBreadcrumbImpl = function(raven, breadcrumb) {
    breadcrumb = exports.extend(
        {
            timestamp: +new Date() / 1000
        },
        breadcrumb
    );
    var currCtx = raven.getContext();
    if (!currCtx.breadcrumbs) currCtx.breadcrumbs = [];
    currCtx.breadcrumbs.push(breadcrumb);
    if (currCtx.breadcrumbs.length > raven.maxBreadcrumbs) {
        currCtx.breadcrumbs.shift();
    }
    raven.setContext(currCtx);
};
