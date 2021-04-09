'use strict'


// Maybe a
function Just(x) {
  this.value = x
  this.isJust = true
}
exports.Just = Just

function Nothing() {
  this.isJust = false
}
exports.Nothing = Object.freeze(new Nothing())

// (b, a -> b) -> Maybe a -> b
function maybe(ifNothing, ifJust) { return function(x) {
  return x.isJust ? ifJust.bind(this)(x.value) : ifNothing
}}
exports.maybe = maybe

// a -> Maybe a -> a
function fromMaybe(x) { return maybe(x, y => y) }
exports.fromMaybe = fromMaybe

// error -> Maybe a -> MonadFail a
function fromJust(error) { return function (x) {
  if (x.isJust) return x.value
  else throw error
}}
exports.fromJust = fromJust

function caseMaybe(justCase, nothingCase) {
  function caseMaybe_internal(x) {
    if (x.isJust) return justCase.bind(this)(x.value)
    return nothingCase.bind(this)(x)
  }
  return caseMaybe_internal.bind(this)
}
exports.caseMaybe = caseMaybe

