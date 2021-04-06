'use strict'

const parsing = require('../parsing')
const {ParseError} = parsing
const {withReader} = require('../RWS')
const {identity, defer, compose, constant, traverse_, foldr} = require('../functional')
const {Just, Nothing, fromJust} = require('../Maybe')
const {Serializer, execSerializer} = require('../serializing')
const {Biparser, genParserSerializer, string, assignProperty} = require('../biparsing')
exports.Biparser = Biparser
exports.ParseError = parsing.ParseError
exports.Serializer = Serializer
exports.execSerializer = execSerializer

// Allows recursive data structures to be constructed without stack overflow
// This is a problem since Biparser is composing functions to construct a parser and serilizer instead of being the parser and serialiser its self. If there is a way to program recursive biparsers like recursive parsers that would be great but right not it seems that the limitations of JavaScript are limiting this ability.
function recurse() {

}
// Recurse example
function log() {return console.log(...arguments)}
function recurseExampleA() {
  log('recurseExampleA')
  const [arg0, arg1] = arguments
  const isRecurseRoot = arg0 === undefined
  log('isRecurseRoot', isRecurseRoot)
  const recurseRoot = isRecurseRoot ? recurseExampleA : arg0

  if (isRecurseRoot) {
    const psLoop = {}
    Object.assign(psLoop, genParserSerializer(function() {
      this.newNamespace()
      const A = 'A'
      this.assignProperty(A, string(A))
      this.assignProperty('recurseA', defer(recurseExampleB)(recurseRoot, psLoop))
    }))

    const {parser, serilizer} = psLoop
    this.pFunction(function(x) {
      log('state a1:', this.get())
      const root = parser.bind(this)(x)
      log('root', root)
      return root
      // const [root, ...subRoots] = this.many(parser)
      // log('root', root)
      // log('subRoots', subRoots)
      // return foldr(([root,leaf]) => { log('foldr', root, leaf); root['recurseA']['recurseB'] = leaf; return root })(root)(subRoots)
    })
  } else if (recurseRoot === recurseExampleA) {
    const psLoop = arg1
    this.pFunction(function(x) {log('state a2:', this.get()); return x})
    this.pFunction(function(x) {
      log('psLoop', psLoop)
      const binded = psLoop.parser.bind(this)
      log('binded', binded, binded.toString())
      try {
        const result = binded(x)
        log('result', result)
        return result
      } catch (e) {
        return e
      }
    })
  }
}
exports.recurseExampleA = recurseExampleA
Biparser.prototype.recurseExampleA = recurseExampleA

function recurseExampleB() {
  log('recurseExampleB')
  const [arg0, arg1] = arguments
  const isRecurseRoot = arg0 === undefined
  log('isRecurseRoot', isRecurseRoot)
  const recurseRoot = isRecurseRoot ? recurseExampleB : arg0

  const psLoop = arg1
  const endProp = 'end'
  const B = 'B'
  this.pFunction(function(x) {log('state b:', this.get()); return x})
  this.alternativeOrError(
    x => Object.prototype.hasOwnProperty.call(x, endProp), assignProperty(endProp, string(endProp)),
    x => Object.prototype.hasOwnProperty.call(x, 'recurseB'), function() {
      this.assignProperty(B, string(B))
      this.assignProperty('recurseB', defer(recurseExampleA)(recurseRoot, psLoop))
    },
    fromJust(new ParseError('all alternatives failed to parse')),
  )
  this.pFunction(function(x) {log('state b end:', this.get()); return x})
}
exports.recurseExampleB = recurseExampleB
Biparser.prototype.recurseExampleB = recurseExampleB

