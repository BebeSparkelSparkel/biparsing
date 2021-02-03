'use strict'

const assert = require('assert')

const {defer, Just, Nothing, fromMaybe} = require('../src/functional')
const {
  genParserSerializer,
  Parser, runParser, evalParser,
  Serializer, execSerializer,
  take, string, optional, many,
  digit,
  } = require('../src/biparsing')


describe('biparsing', function() {
  describe.only('take', function() {
    const biparser = take(2, x => Array.from(x.chars))
    const {parser, serializer} = genParserSerializer(biparser)

    it('parse', function() {
      const s = new Parser('12345')
      assert.deepEqual(runParser(parser, s), ['12','345'])
    })

    it('serialize', function() {
      const w = new Serializer({chars: '123a'})
      assert.deepEqual(execSerializer(serializer, w), '123a')
    })
  })

  describe('string', function() {
    const biparser = string('abc')
    const {parser, serializer} = genParserSerializer(biparser)

    describe('parse', function() {
      it('success', function() {
        assert.deepEqual(runParser(parser, new Parser('abcd')), ['abc','d'])
      })

      it('fail', function() {
        assert.throws(defer(runParser)(parser, new Parser('bcde')))
      })
    })

    it('serialize', function() {
      assert.equal(execSerializer(serializer, new Serializer()), 'abc')
    })
  })

  describe('optional', function() {
    const biparser = optional(string('abc'), x => x === 'def')
    const {parser, serializer} = genParserSerializer(biparser)

    describe('parse', function() {
      it('success', function() {
        const s = new Parser('abcd')
        assert.deepEqual(evalParser(parser, s), new Just('abc'))
      })

      it('fail', function() {
        const s = new Parser('a')
        assert.deepEqual(evalParser(parser, s), Nothing)
      })

    })

    describe('serialize', function() {
      it('does write', function() {
        assert.equal(execSerializer(serializer, new Serializer('def')), 'abc')
      })

      it('does not write', function() {
        assert.equal(execSerializer(serializer, new Serializer('abc')), '')
      })
    })
  })

  describe('many', function() {
    const biparser = many(string('abc'), xs => xs.map(x => (x*2).toString() + ' '))
    const {parser, serializer} = genParserSerializer(biparser)

    it('parse', function() {
      const s = new Parser('abcabcabcdef')
      assert.deepEqual(runParser(parser, s), [['abc','abc','abc'],'def'])
    })

    it('serialize', function() {
      const r = new Reader([1,2,3])
      assert.deepEqual(execSerializer(serializer, r), '2 4 6 ')
    })
  })

  describe('digit', function() {
    const {parser, serializer} = genParserSerializer(digit)

    it('parse', function() {
      assert.deepEqual(runParser(parser, new Parser('1u')), ['1','u'])
      assert.throws(defer(runParser)(parser, new Parser('u1')))
    })
  })

})

