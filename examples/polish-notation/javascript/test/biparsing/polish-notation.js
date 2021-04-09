'use strict'
const assert = require('assert')
const {polishn, operator} = require('../../src/biparsing/polish-notation')
const {genParserSerializer, evalParser, runParser, Parser, execSerializer, Serializer} = require('../../src/biparsing')

describe('polish notation', function() {
  describe('operator', function() {
    const biparser = operator
    const {parser, serializer} = genParserSerializer(biparser)

    const operators = ['+','-','*','/']

    describe('parse', function() {
      operators.forEach(op => it(op, function() {
        assert.equal(evalParser(parser, new Parser(op)), op)
      }))

      it('unknown operator', function() {
        assert.throws(() => runParser(parser, new Parser('^ ')))
      })
    })

    describe('serialize', function() {
      operators.forEach(op => it(op, function() {
        assert.equal(execSerializer(serializer, new Serializer(op)), op)
      }))
    })
  })

  describe('polishn', function() {
    const biparser = polishn
    const {parser, serializer} = genParserSerializer(biparser)

    const simple = '+ 1 2'
    describe(simple, function() {
      const toParse = simple
      const toSerialize = {operator: '+', exprs: [{number: 1}, {number: 2}]}

      it(toParse, function() {
        assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
      })

      it(toParse, function() {
        assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
      })
    })

    const double = '/ 2 + 6 5'
    describe(double, function() {
      const toParse = double
      const toSerialize = {operator: '/', exprs: [{number: 2}, {expr: {operator: '+', exprs: [{number: 6}, {number: 5}]}}]}

      it(toParse, function() {
        assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
      })

      it(toParse, function() {
        assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
      })
    })

  })                 
})                   
                     
                     
                     
                     
