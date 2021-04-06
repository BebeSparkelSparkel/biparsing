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

  // describe('polishn', function() {
  //   const biparser = polishn
  //   const {parser, serializer} = genParserSerializer(biparser)

  //   const simpleToParse = '+ 1 2'
  //   const simpleToSerialize = {operator: '+', exprs: [{number: 1}, {number: 2}]}

  //   describe('parse', function() {
  //     it(simpleToParse, function() {
  //       assert.deepEqual(evalParser(parser, new Parser(simpleToParse)), simpleToSerialize)
  //     })
  //   })

  //   describe('serialize', function() {
  //     it(simpleToParse, function() {
  //       assert.equal(execSerializer(serializer, new Serializer(simpleToSerialize)), simpleToParse)
  //     })
  //   })
  // })
})

