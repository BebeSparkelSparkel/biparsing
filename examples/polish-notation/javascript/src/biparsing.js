'use strict'

const parsing = require('./parsing')
const {tell} = require('./RWS')
const {
  defer, compose, constant,
  Just, Nothing, maybe, fromMaybe,
  foldl, foldr
  } = require('./functional')
const {Parser, ParseError, runParser, evalParser} = require('./parsing')
exports.Parser = Parser
exports.runParser = runParser
exports.evalParser = evalParser
const {Serializer, runSerializer, execSerializer} = require('./serializing')
exports.Serializer = Serializer
exports.runSerializer = runSerializer
exports.execSerializer = execSerializer

// Biparser r a
function Biparser() {
  this.parser = []
  this.serializer = []
}

function genParserSerializer(func) {
  const biparser = new Biparser()
  func.bind(biparser)()
  return {
    parser: compose(...biparser.parser),
    serializer: compose(...biparser.serializer)
  }
}
exports.genParserSerializer = genParserSerializer

// (a -> Bool, a -> String) -> Biparser a
function condition(pred, genErrorMsg) {
  this.parser.unshift(function(x) {
    if (pred(x)) return x
    throw new ParseError(genErrorMsg(x))
  })
}
exports.condition = defer(condition)
Biparser.prototype.condition = condition

// (Int, r -> [Char]) -> Biparser r [Char]
function take(n, serialize) {
  this.parser.unshift(parsing.take(n))
  this.serializer.unshift(takeSerializer(serialize))
}
function takeSerializer(serialize) { return function() {
  const r = this.ask()
  const cs = serialize(r)
  cs.forEach(tell.bind(this))
}}
exports.take = defer(take)
Biparser.prototype.take = take

// // (Char -> Bool, r -> [Char]) -> Biparser r [Char]
// function takeWhile(pred, serialize) {
//   this.parser.unshift(function() {
//     const s = this.get()
//     let n = 0
//     for (const c of s) {
//       if (pred(c)) ++n
//       else break
//     }
//     parser.take(n)
//   })
//   this.serializer.unshift(takeSerializer)
// }

// String -> Biparser r String
function string(s) {
  this.take(s.length, constant(Array.from(s)))
  this.condition(x => x === s, x => `string parser expected "${s}" but received "${x}"`)
}
exports.string = defer(string)
Biparser.prototype.string = string

// (Biparser r a, r -> Bool) -> Biparser r a
function optional(biparser, optSerialize) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.parser.unshift(function(x) {
    try {
      return new Just(parser.bind(this)(x))
    } catch(e) {
      if (e.name === ParseError.name) return Nothing
      throw e
    }
  })
  this.serializer.unshift(function() {
    const r = this.ask()
    if (optSerialize(r)) serializer.bind(this)()
  })
}
exports.optional = defer(optional)
Biparser.prototype.optional = optional

// (Biparser s a, r -> [s]) -> Biparser r [a]
function many(biparser, serialize) {
  // would have rather used the optional biparser recursively but not sure how to accomplish that
  // this.parser.unshift(parsing.many(compose(...biparser.parser)))
  this.serializer.unshift(function() {
    withReader(serialize, function() {
      const xs = this.ask()
      for(x of xs) withReader(constant(x), compose(...biparser.serializer))
    })
  })
}
exports.many = defer(many)
Biparser.prototype.many = many

// Biparser r Digit
function digit() {
  this.take(1, x => [x])
  this.condition(x => '0123456789'.includes(x), x => `digit expected a 0123456789 but received ${x}`)
}
exports.digit = digit
Biparser.prototype.digit = digit

function number() {
  this.optional(string('-'), x => x < 0)
  this.func(x => x.isJust)
  this.assign('isNegative')
  // this.
}
exports.number = number
Biparser.prototype.number = number

