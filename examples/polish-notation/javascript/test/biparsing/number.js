'use strict'

// const assert = require('assert')
const assert = require('assert')

const {defer, constant} = require('../../src/functional')
const {Just, Nothing, maybe, fromMaybe} = require('../../src/Maybe')
const {
  genParserSerializer,
  Parser, runParser, evalParser,
  Serializer, execSerializer,
  condition, pFunction,
  take, string, optional, many, manyN,
  } = require('../../src/biparsing')
const {digit, numberSimple, number, numberZoom} =  require('../../src/biparsing/number')

describe('number', function() {
  describe('digit', function() {
    const biparser = digit
    const {parser, serializer} = genParserSerializer(biparser)

    it('parse', function() {
      assert.deepEqual(runParser(parser, new Parser('1u')), ['1','u'])
      assert.throws(defer(runParser)(parser, new Parser('u1')))
    })
  })

  ;[
    numberSimple,
    number,
    numberZoom,
  ].forEach(function(biparser) {
    describe(biparser.name, function() {
      const {parser, serializer} = genParserSerializer(biparser)

      describe('parse', function() {
        it('positive whole number', function() {
          const s = new Parser('1234a')
          assert.deepEqual(runParser(parser,s), [1234,'a'])
        })

        it('negative whole number', function() {
          const s = new Parser('-1234a')
          assert.deepEqual(runParser(parser,s), [-1234,'a'])
        })

        it('positive decimal number', function() {
          const s = new Parser('1234.5678a')
          assert.deepEqual(runParser(parser,s), [1234.5678,'a'])
        })

        it('negative decimal number', function() {
          const s = new Parser('-1234.5678a')
          assert.deepEqual(runParser(parser,s), [-1234.5678,'a'])
        })
      })

      describe('serialize', function() {
        it('positive whole number', function() {
          const w = new Serializer(1234)
          assert.equal(execSerializer(serializer,w), '1234')
        })

        it('negative whole number', function() {
          const w = new Serializer(-1234)
          assert.equal(execSerializer(serializer,w), '-1234')
        })

        it('positive decimal number', function() {
          const w = new Serializer(1234.5678)
          const result = execSerializer(serializer,w)
          const expected = '1234.5678'
          assert.equal(result.slice(0,expected.length), expected)
        })

        it('negative decimal number', function() {
          const w = new Serializer(-1234.5678)
          const result = execSerializer(serializer,w)
          const expected = '-1234.5678'
          assert.equal(result.slice(0,expected.length), expected)
        })
      })

    })
  })

})

