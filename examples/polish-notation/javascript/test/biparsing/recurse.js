'use strict'

const assert = require('assert')
const deepfreeze = require('deepfreeze')

const {defer, equal, notEqual, identity} = require('../../src/functional')
const {fromJust} = require('../../src/Maybe')
const {
  Biparser, genParserSerializer,
  Parser, evalParser, ParseError,
  Serializer, execSerializer,
  string, atLeast1, assignProperty,
} = require('../../src/biparsing')
const {recurse} = require('../../src/biparsing/recurse')


function hasOwnProperty(propertyName) { return function(object) { return Object.prototype.hasOwnProperty.call(object, propertyName) } }

describe('recursive biparsing', function() {
  describe('examples', function() {
    describe('simple recursive biparser example where the biparsers are mutually recursive and either may be used as the root biparser', function() {
      const recurseExampleA = recurse(function recurseExampleA_biparser() {
        const args = arguments
        this.newNamespace()
        this.assignProperty('a', string('A'))
        this.assignProperty('recurseA', defer(recurseExampleB)(...args))
      })
      Biparser.prototype.recurseExampleA = recurseExampleA

      const recurseExampleB = recurse(function recurseExampleB_biparser() {
        const args = arguments
        const endProp = 'end'
        this.alternativeOrError(
          hasOwnProperty(endProp), assignProperty(endProp, string(endProp)),
          hasOwnProperty('recurseB'), function() {
            this.assignProperty('b', string('B'))
            this.assignProperty('recurseB', defer(recurseExampleA)(...args))
          },
          fromJust(new ParseError('all alternatives failed to parse')),
        )
      })
      Biparser.prototype.recurseExampleB = recurseExampleB

      describe('A', function() {
        it('no stack overflow with construction', function() {
          const {parser, serializer} = genParserSerializer(recurseExampleA)
          assert.equal(typeof parser, 'function')
          assert.equal(typeof serializer, 'function')
        })

        const toParse = 'ABAend'
        const toSerialize = deepfreeze({a: 'A', recurseA: {b: 'B', recurseB: {a: 'A', recurseA: {end: 'end'}}}})

        it('parse', function() {
          const {parser} = genParserSerializer(recurseExampleA)
          assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
        })

        it('serialize', function() {
          const {serializer} = genParserSerializer(recurseExampleA)
          assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
        })

      })

      describe('B', function() {
        it('no stack overflow with construction', function() {
          const {parser, serializer} = genParserSerializer(recurseExampleB)
          assert.equal(typeof parser, 'function')
          assert.equal(typeof serializer, 'function')
        })

        const toParse = 'BAend'
        const toSerialize = deepfreeze({b: 'B', recurseB: {a: 'A', recurseA: {end: 'end'}}})

        it('parse', function() {
          const {parser} = genParserSerializer(recurseExampleB)
          assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
        })

        it('serialize', function() {
          const {serializer} = genParserSerializer(recurseExampleB)
          assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
        })

      })
    })

    describe('works with Biparser.prototype.atLeast1', function() {
      const ra = recurse(function raB() {
        const args = arguments
        this.newNamespace()
        this.assignProperty('a', string('A'))
        this.assignProperty('atLeast1', atLeast1(defer(rb)(...args), identity))
      })
      const rb = recurse(function rbB() {
        const args = arguments
        this.alternativeOrError(
          equal('B'), string('B'),
          notEqual('B'), defer(ra)(...args),
          new ParseError('rb parser failed'),
        )
      })

      const toParse = 'ABBBAB'
      const toSerialize = deepfreeze({a: 'A', atLeast1: ['B','B','B',{a: 'A', atLeast1: ['B']}]})

      describe('A', function() {
        it('no stack overflow with construction', function() {
          const {parser, serializer} = genParserSerializer(ra)
          assert.equal(typeof parser, 'function')
          assert.equal(typeof serializer, 'function')
        })

        it('parse', function() {
          const {parser} = genParserSerializer(ra)
          assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
        })

        it('serialize', function() {
          const {serializer} = genParserSerializer(ra)
          assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
        })
      })

      describe('B', function() {
        it('no stack overflow with construction', function() {
          const {parser, serializer} = genParserSerializer(rb)
          assert.equal(typeof parser, 'function')
          assert.equal(typeof serializer, 'function')
        })

        it('parse', function() {
          const {parser} = genParserSerializer(rb)
          assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
        })

        it('serialize', function() {
          const {serializer} = genParserSerializer(rb)
          assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
        })

      })
    })
  })
})

