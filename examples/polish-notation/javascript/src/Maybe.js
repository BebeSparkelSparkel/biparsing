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

