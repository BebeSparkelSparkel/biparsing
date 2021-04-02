'use strict'

const {State} = require('./RWS')
const {defer, foldl, foldr} = require('./functional')
const {Just, Nothing, maybe, fromMaybe} = require('./Maybe')


function Parser(tokens) {
  this.state = tokens
}
Object.assign(Parser.prototype, State.prototype)
exports.Parser = Parser

// (ParserModifier, Parser s) -> [a,s]
function runParser(func, parser) {
  const newParser = new Parser(parser.state)
  return [func.bind(newParser)(), newParser.state]
}
exports.runParser = runParser

/// (ParserModifier, Parser s) -> a
function evalParser(func, parser) {
  const [x] = runParser(func, parser)
  return x
}
exports.evalParser = evalParser

function ParseError(message = '') {
  this.name = 'ParseError'
  this.message = message
}
exports.ParseError = ParseError
ParseError.prototype = Error.prototype

// Int -> State String String
function take(n) {
  const x = this.get()
  const head = x.slice(0,n)
  if (head.length < n) throw new ParseError(`take could not consume ${n} tokens`)
  this.put(x.slice(n))
  return head
}
exports.take = defer(take)
Parser.prototype.take = take

function string(s) {
  const head = this.take(s.length)
  if (head === s) return head
  throw new ParseError(`string parser could not match string "${s}"`)
}
exports.string = defer(string)
Parser.prototype.string = string

// Parser a -> [a]
function many(parser) {
  return maybe(
    [],
    function(x) {
      const accumulated = this.many(parser)
      accumulated.unshift(x)
      return accumulated
    },
  ).bind(this)(this.optional(parser))
}
exports.many = defer(many)
Parser.prototype.many = many

function many1(parser) {
  const xs = this.many(parser)
  if (xs.length > 0) return xs
  throw new ParseError('many1 could not parse at least one item')
}
exports.many1 = defer(many1)
Parser.prototype.many1 = many1

function optional(parser) {
  const s = this.get()
  try {
    return new Just(parser.bind(this)())
  } catch (e) {
    if (e.name === ParseError.name) {
      this.put(s)
      return Nothing
    }
    throw e
  }
}
exports.optional = defer(optional)
Parser.prototype.optional = optional

function digit() {
  const c = this.take(1)
  if ('0123456789'.includes(c)) return c
  throw new ParseError(`digit expected a 0123456789 but received ${c}`)
}
Parser.prototype.digit = digit
exports.digit = digit

function number() {
  const isNegative = fromMaybe(false)(this.optional(defer(string)('-')))
  const wholeDigits = this.many1(digit)
  const wholeValue = foldl((x,y) => x * 10 + Number.parseInt(y))(0)(wholeDigits)
  const fractionalValue = fromMaybe(0)(this.optional(function() {
    this.string('.')
    const fractionalDigits = this.many1(digit)
    const fractionalValue = foldr((x,y) => Number.parseInt(x) + y / 10)(0)(fractionalDigits) / 10
    return fractionalValue
  }))
  return (isNegative ? -1 : 1) * (wholeValue + fractionalValue)
}
exports.number = number
Parser.prototype.number = number

