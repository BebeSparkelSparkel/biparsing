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

function log() {return console.log(...arguments)}


// Allows recursive data structures to be constructed without stack overflow
// This is a problem since Biparser is composing functions to construct a parser and serializer instead of being the parser and serialiser its self. If there is a way to program recursive biparsers like recursive parsers that would be great but right not it seems that the limitations of JavaScript are limiting this ability.
// Allows entry from any part of the recurse loop.
// See examples in test/biparsing/recurse.js
function recurse(biparser) { return function() {
  const [arg0, arg1] = arguments
  const isRecurseRoot = arg0 === undefined
  const recurseRoot = isRecurseRoot ? biparser : arg0
  if (isRecurseRoot) {
    const psLoop = new Object()
    Object.assign(psLoop, genParserSerializer(defer(biparser)(recurseRoot, psLoop)))
    const {parser, serializer} = psLoop
    this.pFunction(parser)
    this.sFunction(serializer)
  } else if (recurseRoot === biparser) {
    const psLoop = arg1
    this.pFunction(function(x) { try { return psLoop.parser.bind(this)(x) } catch (e) { return e } })
    this.sFunction(function(x) { return psLoop.serializer.bind(this)(x) })
  } else {
    biparser.bind(this)(...arguments)
  }
}}
exports.recurse = recurse

