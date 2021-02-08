'use strict'

const parsing = require('./parsing')
const {ParseError} = parsing
const {tell, withReader} = require('./RWS')
const { identity, defer, compose, constant, foldl, foldr, traverse_ } = require('./functional')
const { Just, Nothing, fromMaybe } = require('./Maybe')
const {Serializer, runSerializer, execSerializer} = require('./serializing')
exports.Serializer = Serializer
exports.runSerializer = runSerializer
exports.execSerializer = execSerializer


// Biparser r a
function Biparser() {
  this.parser = []
  this.serializer = []
}
exports.Biparser = Biparser

function Parser(tokens) {
  this.state = tokens
  this.assignments = {}
}
exports.Parser = Parser
Parser.prototype = parsing.Parser.prototype

// (ParserModifier, Parser s) -> [a,s]
function runParser(func, parser) {
  const newParser = new Parser(parser.state)
  newParser.assignments = parser.assignments
  return [func.bind(newParser)(), newParser.state]
}
exports.runParser = runParser

// (ParserModifier, Parser s) -> a
function evalParser(func, state) {
  const [x] = runParser(func, state)
  return x
}
exports.evalParser = evalParser

function genParserSerializer(func) {
  const biparser = new Biparser()
  func.bind(biparser)()
  return {
    parser: compose(biparser.parser),
    serializer: compose(biparser.serializer)
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

// Biparser type needs to be updated to show how the function changes the Biparser type
// (a -> b) -> Biparser r b
function pFunction(func) { this.parser.unshift(func) }
exports.pFunction = defer(pFunction)
Biparser.prototype.pFunction = pFunction

// (r -> [Char]) -> Biparser r String
function takeDynamic(serialize) {
  this.parser.unshift(function(n) {
    const x = this.get()
    const head = x.slice(0,n)
    if (head.length < n) throw new ParseError(`take could not consume ${n} tokens`)
    this.put(x.slice(n))
    return head
  })
  this.serializer.unshift(function() {
    const r = this.ask()
    this.tell(serialize(r))
  })
}
exports.takeDynamic = defer(takeDynamic)
Biparser.prototype.takeDynamic = takeDynamic

// (Int, r -> [Char]) -> Biparser r String
function take(n, serialize) {
  this.pFunction(constant(n))
  this.takeDynamic(serialize)
}
exports.take = defer(take)
Biparser.prototype.take = take

// (Char -> Bool, r -> [Char]) -> Biparser r String
function takeWhile(pred, serialize) {
  this.parser.unshift(function() {
    const s = this.get()
    let n = 0
    for (const c of s) {
      if (pred(c)) ++n
      else break
    }
    return n
  })
  this.takeDynamic(serialize)
}
exports.takeWhile = defer(takeWhile)
Biparser.prototype.takeWhile = takeWhile

// String -> Biparser r String
function string(s) {
  this.take(s.length, constant(s))
  this.condition(x => x === s, x => `string parser expected "${s}" but received "${x}"`)
}
exports.string = defer(string)
Biparser.prototype.string = string

// (r -> Bool, Biparser r a) -> Biparser r (Maybe a)
function optional(optSerialize, biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.parser.unshift(function(x) {
    const s = this.get()
    try {
      return new Just(parser.bind(this)(x))
    } catch(e) {
      this.put(s)
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

// (Biparser r' a, r -> [r']) -> Biparser r [a]
function many(biparser, serialize) {
  const {parser, serializer} = genParserSerializer(biparser)
  // would have rather used the optional biparser recursively but not sure how to accomplish that
  this.zoom(serialize, function() {
    this.parser.unshift(parsing.many(parser))
    this.serializer.unshift(function() {
      const xs = this.ask()
      traverse_(function(x) {
        this.withReader(constant(x), serializer)
      }).bind(this)(xs)
    })
  })
}
exports.many = defer(many)
Biparser.prototype.many = many

function manyN(n, biparser, serialize) {
  this.many(biparser, serialize)
  this.condition(x => x.length > 0, x => constant(`manyN expected at least ${n} parsed elements but only parsed ${x.length} elements`))
}
exports.manyN = defer(manyN)
Biparser.prototype.manyN = manyN

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// String -> Biparser r undefined
function assign(name) {
  this.parser.unshift(function(x) {
    this.assignments[name] = x
  })
}
exports.assign = defer(assign)
Biparser.prototype.assign = assign

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// String -> Biparser r a
function reference(name) {
  this.parser.unshift(function() {
    return this.assignments[name]
  })
}
exports.reference = defer(reference)
Biparser.prototype.reference = reference

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// Biparser r {..}
function referenceAll() {
  this.parser.unshift(function() {
    return this.assignments
  })
}
exports.referenceAll = defer(referenceAll)
Biparser.prototype.referenceAll = referenceAll

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// Biparser r a -> Biparser r a
function newAssignmentSpace(biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.parser.unshift(function(x) {
    const superAssignments = this.assignments
    this.assignments = {}
    const y = parser.bind(this)(x)
    this.assignments = superAssignments
    return y
  })
  this.serializer.unshift(serializer)
}
exports.newAssignmentSpace = defer(newAssignmentSpace)
Biparser.prototype.newAssignmentSpace = newAssignmentSpace

// ('r -> r, Biparser r a) -> Biparser r' a
function zoom(z, biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.parser.unshift(parser)
  this.serializer.unshift(withReader(z, serializer))
}
exports.zoom = defer(zoom)
Biparser.prototype.zoom = zoom

