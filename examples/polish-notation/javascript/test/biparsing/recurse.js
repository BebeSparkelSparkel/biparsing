'use strict'

const assert = require('assert')
const deepfreeze = require('deepfreeze')

const {defer} = require('../../src/functional')
const {fromJust} = require('../../src/Maybe')
const {
  Biparser, genParserSerializer,
  Parser, evalParser, ParseError,
  Serializer, execSerializer,
  string, assignProperty,
} = require('../../src/biparsing')
const {recurse} = require('../../src/biparsing/recurse')


describe('recursive biparsing', function() {
  describe('example', function() {

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
        x => Object.prototype.hasOwnProperty.call(x, endProp), assignProperty(endProp, string(endProp)),
        x => Object.prototype.hasOwnProperty.call(x, 'recurseB'), function() {
          this.assignProperty('b', string('B'))
          // this.assignProperty('recurseB', defer(recurseExampleA)(...arguments))
          this.assignProperty('recurseB', function() {return recurseExampleA.bind(this)(...args)})
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
})

