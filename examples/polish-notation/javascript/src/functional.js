'use strict'

function identity(x) { return x }
exports.identity = identity

function constant(x) { return function () {return x} }
exports.constant = constant

function compose() {
  const as = Array.from(arguments)
  return function(x) {return foldr((f,y) => f.bind(this)(y), x, as)}
}
exports.compose = compose

function foldl(f, x, xs) {
  if (xs.length <= 0) return x
  const [y,...ys] = xs
  return foldl(f, f(x,y), ys)
}
exports.foldl = foldl

function foldr(f, x, xs) {
  if (xs.length <= 0) return x
  const [y,...ys] = xs
  return f(y, foldr(f, x, ys))
}
exports.foldr = foldr

function defer(f) {
  return function(...xs) {
  return function() {
  return f.bind(this)(...xs)
} } }
exports.defer = defer

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
// (a -> b, () -> b) -> Maybe a -> b
function maybe(ifJust, ifNothing) {
  return function(x) {
    return x.isJust ? ifJust.bind(this)(x.value) : ifNothing.bind(this)()
  }
}
exports.maybe = maybe
function fromMaybe(x) { return maybe(identity, constant(x)) }
exports.fromMaybe = fromMaybe

