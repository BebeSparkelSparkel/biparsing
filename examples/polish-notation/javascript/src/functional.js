'use strict'

const {maybe} = require('./Maybe')

function identity(x) { return x }
exports.identity = identity

function constant(x) { return function () {return x} }
exports.constant = constant

// [c -> d, ..., a -> b] -> a -> d
function compose(as) {
  if (!Array.isArray(as)) as = arguments
  return function(x) {
  return foldr((f,y) => f.bind(this)(y))(x)(as)
}}
exports.compose = compose

// (a -> b) -> a -> b
function defer(f) {
  return function(...xs) {
  return function() {
  return f.bind(this)(...xs)
} } }
exports.defer = defer

// (b,a) -> b -> b -> [a] -> b
function foldl(f) { return function(x) { return function(xs) {
  if (xs.length <= 0) return x
  const [y,...ys] = xs
  return foldl(f)(f(x,y))(ys)
}}}
exports.foldl = foldl

// (a,b) -> b -> b -> [a] -> b
function foldr(f) { return function(x) { return function(xs) {
  if (xs.length <= 0) return x
  const [y,...ys] = xs
  return f(y, foldr(f)(x)(ys))
}}}
exports.foldr = foldr

// (a -> (b, Maybe a)) -> a -> NonEmpty b
function unfold(uf) { return function(x) {
  const [y,z] = uf(x)
  const accumulated = maybe([], unfold(uf))(z)
  accumulated.unshift(y)
  return accumulated
}}
exports.unfold = unfold

// Monad m => ((b,a) -> m b) -> b -> [a] -> m b
function foldM(f) { return function(x) { return function(xs) {
  if (xs.length <= 0) return x
  const [y,...ys] = xs
  return foldM(f)(f(x,y))(ys)
}}}
exports.foldM = foldM

// (a -> f b) -> [a] -> f undefined
function traverse_(f) { return function(xs) {
  for (const x of xs) f.bind(this)(x)
}}
exports.traverse_ = traverse_

function map(f) {return function(xs) { return xs.map(f) } }
exports.map = map
