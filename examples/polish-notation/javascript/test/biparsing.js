'use strict'

const assert = require('assert')
const deepfreeze = require('deepfreeze')

const {identity, compose, defer, constant, map} = require('../src/functional')
const {Just, Nothing} = require('../src/Maybe')
const {
  genParserSerializer,
  Parser, runParser, evalParser,
  Serializer, execSerializer,
  take, string, optional, many, alternative,
} = require('../src/biparsing')


describe('biparsing', function() {
  describe('take', function() {
    const biparser = take(2, x => x.chars)
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
    const biparser = optional(x => x === 'def', string('abc'))
    const {parser, serializer} = genParserSerializer(biparser)

    describe('parse', function() {
      it('success', function() {
        const s = new Parser('abcd')
        assert.deepEqual(evalParser(parser, s), new Just('abc'))
      })

      it('fail', function() {
        const s = new Parser('cba')
        assert.deepEqual(runParser(parser, s), [Nothing,'cba'])
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
    const biparser = many(
      take(3, x => x.join('')),
      compose(
        map(x => x.split('').reverse()),
        ({a}) => a,
      ),
    )
    const {parser, serializer} = genParserSerializer(biparser)

    it('parse', function() {
      const s = new Parser('abcdefhijkl')
      assert.deepEqual(runParser(parser, s), [['abc','def','hij'],'kl'])
    })

    it('serialize', function() {
      const r = new Serializer({a: ['cba','fed','jih']})
      assert.equal(execSerializer(serializer, r), 'abcdefhij')
    })
  })

  describe('alternative', function() {
    const biparser = alternative(x => x === 'a', string('a'), x => x === 'b', string('b'))
    const {parser, serializer} = genParserSerializer(biparser)

    it('parse', function() {
      assert.deepEqual(runParser(parser, new Parser('ab')), [new Just('a'),'b'])
      assert.deepEqual(runParser(parser, new Parser('bb')), [new Just('b'),'b'])
      assert.deepEqual(runParser(parser, new Parser('ba')), [new Just('b'),'a'])
    })

    it('serialize', function() {
      assert.equal(execSerializer(serializer, new Serializer('a')), 'a')
      assert.equal(execSerializer(serializer, new Serializer('b')), 'b')
      assert.equal(execSerializer(serializer, new Serializer('c')), '')
    })
  })

  describe('asssignments', function() {
    it('newAssignmentSpace', function() {
      const biparser = function() {
        this.string('1')
        this.assign('a')
        this.string('2')
        this.assign('b')
        this.newAssignmentSpace(function() {
          this.string('3')
          this.assign('a')
          this.string('4')
          this.assign('b')
          this.referenceAll()
        })
        this.assign('c')
        this.referenceAll()
      }
      const {parser} = genParserSerializer(biparser)
      const s = new Parser('123456')
      assert.deepEqual(
        runParser(parser,s),
        [{a: 1,b: 2,c: {a: 3,b: 4}},'56'],
      )
    })

    it('newAssignmentSpace optional', function() {
      const biparser = function() {
        this.optional(constant(true), string('1'))
        this.assign('a')
        this.newAssignmentSpace(function() {
          this.optional(constant(true), string('2'))
          this.assign('a')
          this.referenceAll()
        })
        this.assign('b')
        this.referenceAll()
      }
      const {parser} = genParserSerializer(biparser)
      const s = new Parser('123')
      assert.deepEqual(
        runParser(parser,s),
        [{a: new Just('1'), b: {a: new Just('2')}}, '3'],
      )
    })

    it('newAssignmentSpace at start', function() {
      function biparser() { this.newAssignmentSpace(function() {
        this.string('-')
      })}
      const {parser} = genParserSerializer(biparser)
      const s = new Parser('-abc')
      assert.deepEqual(
        runParser(parser,s),
        ['-','abc'],
      )
    })
  })

  describe('assignProperty', function() {
    const biparser = function() {
      this.assignProperty('abc', take(3, identity))
      this.assignProperty('def', take(3, identity))
    }
    const {parser, serializer} = genParserSerializer(biparser)

    const toParse = 'ghijkl'
    const toSerialize = deepfreeze({abc: 'ghi', def: 'jkl'})

    it('parse', function() {
      assert.deepEqual(evalParser(parser, new Parser(toParse)), toSerialize)
    })

    it('serialize', function() {
      assert.equal(execSerializer(serializer, new Serializer(toSerialize)), toParse)
    })
  })
})

