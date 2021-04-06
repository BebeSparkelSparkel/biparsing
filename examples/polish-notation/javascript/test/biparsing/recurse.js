'use strict'

const assert = require('assert')
const deepfreeze = require('deepfreeze')

const {identity, compose, defer, constant, map} = require('../../src/functional')
const {Just, Nothing} = require('../../src/Maybe')
const {
  genParserSerializer,
  Parser, runParser, evalParser,
  Serializer, execSerializer,
  take, string, optional, many, alternative,
} = require('../../src/biparsing')
const {recurseExampleA, recurseExampleB } = require('../../src/biparsing/recurse')


describe('recursive biparsing', function() {
  describe('example', function() {
    it('no stack overflow with construction', function() {
      const {parser: pA, serializer: sA} = genParserSerializer(recurseExampleA)
      assert.equal(typeof pA, 'function')
      assert.equal(typeof sA, 'function')
      // const {parser: pB, serializer: sB} = genParserSerializer(recurseExampleB)
      // assert.equal(typeof pB, 'function')
      // assert.equal(typeof sB, 'function')
    })

    const toParse = 'ABAend'
    const toSerialize = deepfreeze({A: 'A', recurseA: {B: 'B', recurseB: {A: 'A', recurseA: {end: 'end'}}}})

    it.only('parse', function() {
      const {parser} = genParserSerializer(recurseExampleA)
      assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
    })

    it('serialize', function() {
      const {serializer} = genParserSerializer(recurseExampleA)
      assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
    })
  })
})

