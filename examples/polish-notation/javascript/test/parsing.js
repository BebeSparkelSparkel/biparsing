'use strict'

const assert = require('assert')
const {Just, Nothing, fromMaybe} = require('../src/Maybe')

const {
  Parser, runParser, evalParser,
  optional, string, atLeastN,
  digit, number,
} = require('../src/parsing')

describe('parsing', function() {
  it('take', function() {
    const s = new Parser('012345')
    function takeSome() {
      const t0 = this.take(1)
      const t1 = this.take(3)
      return [t0,t1]
    }
    assert.deepEqual(runParser(takeSome, s), [['0','123'],'45'])
  })

  it('string', function() {
    const s = new Parser('abc123')
    assert.deepEqual(runParser(string('abc'), s), ['abc','123'])
  })

  describe('optional', function() {
    it('success', function() {
      const s = new Parser('1')
      assert.deepEqual(evalParser(optional(digit), s), new Just('1'))
    })

    it('fail', function() {
      const s = new Parser('a')
      assert.deepEqual(evalParser(optional(digit), s), Nothing)
    })

    it('within larger parser', function() {
      function containsOptional() {
        const d = this.optional(digit)
        return fromMaybe('not 2')(d)
      }
      assert.equal(evalParser(containsOptional, new Parser('2')), '2')
      assert.equal(evalParser(containsOptional, new Parser('a')), 'not 2')
    })
  })

  describe('atLeastN', function() {
    it('has exact amount', function() {
      const parser = atLeastN(3)(digit)
      assert.deepEqual(runParser(parser, new Parser('123a')), [['1','2','3'],'a'])
    })

    it('has more', function() {
      const parser = function() {
        return this.atLeastN(3)(digit)
      }
      assert.deepEqual(runParser(parser, new Parser('12345a')), [['1','2','3','4','5'],'a'])
    })

    it('not enough', function() {
      assert.throws(() => runParser(atLeastN(3)(digit), new Parser('12a')))
    })
  })

  it('many', function() {
    const s = new Parser('123a')
    function manyGetSome() {
      const digits = this.many(digit)
      return digits
    }
    assert.deepEqual(runParser(manyGetSome, s), [['1','2','3'],'a'])
  })

  it('digit', function() {
    const s = new Parser('1a')
    assert.deepEqual(runParser(digit, s), ['1','a'])
    assert.equal(evalParser(digit, s), '1')
  })

  describe('number', function() {
    it('whole', function() {
      const s = new Parser('1234')
      assert.equal(evalParser(number, s), 1234)
    })

    it('float', function() {
      const s = new Parser('1234.5678')
      assert.equal(evalParser(number, s), 1234.5678)
    })

    it('negative float', function() {
      const s = new Parser('-234.5678')
      assert.equal(evalParser(number, s), -234.5678)
    })
  })

})

