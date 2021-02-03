'use strict'
const assert = require('assert')

const {
  Reader, runReader, withReader, ask,
  Writer, runWriter,
  State, runState,
  RWS, runRWS
  } = require('../src/RWS.js')


describe('RWS', function() {
  describe('Reader', function() {
    it('returns a modified reader value', function() {
      const r = new Reader('abc')
      function doSomeReaderStuff() {
        const x = this.ask()
        return x + 'def'
      }
      assert.equal(runReader(doSomeReaderStuff, r), 'abcdef')
    })

    it('withReader', function() {
      const r = new Reader({a:1,b:2})
      function withReaderTest() {
        const x = this.ask()
        const y = this.withReader(x => x.b, ask)
        return [x,y]
      }
      assert.deepEqual(runReader(withReaderTest, r), [{a:1,b:2},2])
    })
  })

  describe('Writer', function() {
    it('writes a few items', function() {
      const w = new Writer()
      function writesAFewItems() {
        this.tell('abc')
        this.tell('def')
        return 'ghi'
      }
      assert.deepEqual(runWriter(writesAFewItems, w), ['ghi', 'abcdef'])
    })
  })

  describe('State', function() {
    describe('get put', function() {
      it('get what is put', function() {
        const s = new State('abc')
        function stateChange () {
          const initState = this.get()
          this.put('def')
          return initState
        }
        assert.deepEqual(runState(stateChange, s), ['abc','def'])
      })
    })
  })

    describe('RWS', function() {
      it('read write change state', function() {
        const rws = new RWS('reader string', 'init state')
        function rwsAll() {
          const readerString = this.ask()
          this.tell(readerString)
          const stateInitString = this.get()
          this.put('changed state')
          this.tell(' ')
          this.tell(stateInitString)
          return 'return type'
        }
        assert.deepEqual(runRWS(rwsAll, rws), ['return type', 'changed state', 'reader string init state'])
      })
    })

})

