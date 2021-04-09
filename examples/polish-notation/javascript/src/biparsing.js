'use strict'

const parsing = require('./parsing')
const {ParseError} = parsing
const {withReader} = require('./RWS')
const {identity, defer, compose, constant, traverse_, equal} = require('./functional')
const {Just, Nothing, fromJust} = require('./Maybe')
const {Serializer, execSerializer} = require('./serializing')
exports.ParseError = parsing.ParseError
exports.Serializer = Serializer
exports.execSerializer = execSerializer


// Biparser r a
function Biparser() {
  this.parser = identity
  this.serializer = identity
}
exports.Biparser = Biparser

function Parser(tokens) {
  this.state = tokens
  this.assignments = {}
}
exports.Parser = Parser
Object.assign(Parser.prototype, parsing.Parser.prototype)

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
  return biparser
}
exports.genParserSerializer = genParserSerializer

// (a -> Bool, a -> String) -> Biparser a
function condition(pred, genErrorMsg) {
  this.pFunction(function(x) {
    if (pred(x)) return x
    throw new ParseError(genErrorMsg(x))
  })
}
exports.condition = defer(condition)
Biparser.prototype.condition = condition

// Biparser type needs to be updated to show how the function changes the Biparser type
// (a -> b) -> Biparser r b
function pFunction(func) {this.parser = compose(func, this.parser)}
exports.pFunction = defer(pFunction)
Biparser.prototype.pFunction = pFunction

function sFunction(func) {this.serializer = compose(func, this.serializer)}
exports.sFunction = defer(sFunction)
Biparser.prototype.sFunction = sFunction

// (r -> [Char]) -> Biparser r String
function takeDynamic(serialize) {
  this.pFunction(function(n) {
    const x = this.get()
    const head = x.slice(0,n)
    if (head.length < n) throw new ParseError(`take could not consume ${n} tokens`)
    this.put(x.slice(n))
    return head
  })
  this.sFunction(function() {
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
  this.pFunction(function() {
    const s = this.get()
    let n = 0
    for (const c of s) {
      if (pred(c)) n += 1
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

// Biparser r String
function spaces(numberSerializedSpaces) {
  this.takeWhile(
    equal(' '),
    constant([' '.repeat(numberSerializedSpaces)]),
  )
}
exports.spaces = defer(spaces)
Biparser.prototype.spaces = spaces

// (r -> Bool, Biparser r a) -> Biparser r (Maybe a)
function optional(optSerialize, biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.pFunction(function(x) {
    const s = this.get()
    try {
      return new Just(parser.bind(this)(x))
    } catch (e) {
      this.put(s)
      if (e.name === ParseError.name) return Nothing
      throw e
    }
  })
  this.sFunction(function() {
    const r = this.ask()
    if (optSerialize(r)) serializer.bind(this)()
  })
}
exports.optional = defer(optional)
Biparser.prototype.optional = optional

function atLeastN(n) { return function(biparser, serialize) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.zoom(serialize, function() {
    this.pFunction(parsing.atLeastN(n)(parser))
    this.sFunction(function() {
      const xs = this.ask()
      traverse_(function(x) {
        this.withReader(constant(x), serializer)
      }).bind(this)(xs)
    })
  })

  // this.many(biparser, serialize)
  // this.condition(x => x.length > 0, x => constant(`atLeastN expected at least ${n} parsed elements but only parsed ${x.length} elements`))
}}
exports.atLeastN = compose(defer, atLeastN)
Biparser.prototype.atLeastN = atLeastN

const atLeast1 = atLeastN(1)
exports.atLeast1 = defer(atLeast1)
Biparser.prototype.atLeast1 = atLeast1

// // (Biparser r' a, r -> [r']) -> Biparser r [a]
// function many(biparser, serialize) {
//   const {parser, serializer} = genParserSerializer(biparser)
//   // would have rather used the optional biparser recursively but not sure how to accomplish that
//   this.zoom(serialize, function() {
//     this.pFunction(parsing.many(parser))
//     this.sFunction(function() {
//       const xs = this.ask()
//       traverse_(function(x) {
//         this.withReader(constant(x), serializer)
//       }).bind(this)(xs)
//     })
//   })
// }
// exports.many = defer(many)
// Biparser.prototype.many = many
const many = atLeastN(0)
exports.many = defer(many)
Biparser.prototype.many = many

// r -> Bool -> Biparser r a) -> r -> Bool -> Biparser r a -> Biparser r (Maybe a)
function alternative(xPred, xBi, yPred, yBi) {
  const {parser: xParser, serializer: xSerializer} = genParserSerializer(xBi)
  const {parser: yParser, serializer: ySerializer} = genParserSerializer(yBi)
  this.pFunction(function(x) {
    const xResult = this.optional(defer(xParser)(x))
    return xResult.isJust ? xResult : this.optional(defer(yParser)(x))
  })
  this.sFunction(function() {
    const x = this.ask()
    if (xPred(x)) xSerializer.bind(this)()
    else if (yPred(x)) ySerializer.bind(this)()
  })
}
exports.alternative = defer(alternative)
Biparser.prototype.alternative = alternative

// Like alternative but throws error if all parses fail
// r -> Bool -> Biparser r a) -> r -> Bool -> Biparser r a -> error -> Biparser r (Maybe a)
function alternativeOrError(xPred, xBi, yPred, yBi, error) {
  this.alternative(xPred, xBi, yPred, yBi)
  this.pFunction(fromJust(error))
}
exports.alternativeOrError = defer(alternativeOrError)
Biparser.prototype.alternativeOrError = alternativeOrError

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// String -> Biparser r undefined
function assign(name) {
  this.pFunction(function(x) {
    this.assignments[name] = x
  })
}
exports.assign = defer(assign)
Biparser.prototype.assign = assign

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// String -> Biparser r a
function reference(name) {
  this.pFunction(function() {
    return this.assignments[name]
  })
}
exports.reference = defer(reference)
Biparser.prototype.reference = reference

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// Biparser r {..}
function referenceAll() {
  this.pFunction(function() {
    return this.assignments
  })
}
exports.referenceAll = defer(referenceAll)
Biparser.prototype.referenceAll = referenceAll

// Biparser type needs to be updated to show how the assignment changes the Biparser type
// Biparser r a -> Biparser r a
function newAssignmentSpace(biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.pFunction(function(x) {
    const superAssignments = this.assignments
    this.assignments = {}
    const y = parser.bind(this)(x)
    this.assignments = superAssignments
    return y
  })
  this.sFunction(serializer)
}
exports.newAssignmentSpace = defer(newAssignmentSpace)
Biparser.prototype.newAssignmentSpace = newAssignmentSpace

// Assign to object properyty while parsing and extract property value while serializing
// (String :: propName) -> Biparser r a -> Biparser {} {propName: a}
function assignProperty(propName, biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.pFunction(function(x) {
    const newObj = Object.assign({}, x)
    newObj[propName] = parser.bind(this)()
    return newObj
  })
  this.sFunction(withReader(x => x[propName], serializer))
}
exports.assignProperty = defer(assignProperty)
Biparser.prototype.assignProperty = assignProperty

// Return a new object while parsing. Used with assignProperty when a new namespace is needed.
// Biparser a {}
function newNamespace() {
  this.pFunction(constant({}))
}
exports.newNamespace = newNamespace
Biparser.prototype.newNamespace = newNamespace

// ('r -> r, Biparser r a) -> Biparser r' a
function zoom(z, biparser) {
  const {parser, serializer} = genParserSerializer(biparser)
  this.pFunction(parser)
  this.sFunction(withReader(z, serializer))
}
exports.zoom = defer(zoom)
Biparser.prototype.zoom = zoom

