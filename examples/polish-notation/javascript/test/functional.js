'use strict'
const assert = require('assert')

const {compose, foldl, foldr, unfold, foldM, defer} = require('../src/functional.js')
const {Just, Nothing} = require('../src/Maybe')


describe('functional', function() {
  it('compose', function() {
    assert.equal(compose([x => x / 10, x => x / 2])(100), 5)
    assert.equal(compose([x => x / 10])(100), 10)
  })

  it('defer', function() { assert.equal(
    defer((x,y,z) => x * y * z)(2,3,4)(),
    24
  )})

  it('foldl', function() { assert.equal(
    foldl((x,y) => x / y)(100)([10,2]),
    5
  )})

  it('foldr', function() { assert.equal(
    foldr((x,y) => x / y)(2)([100, 10]),
    20
  )})

  it('unfold', function() { assert.deepEqual(
    unfold(x => [x, x < 5 ? new Just(x*2) : Nothing])(1),
    [1,2,4,8]
  )})

  it('foldM', function() {
    const globalAccumulator = []
    function f(x,y) {
      this.push(x)
      return x / y
    }
    assert.equal(
      foldM(f.bind(globalAccumulator))(24)([2,3,4]),
      1
    )
    assert.deepEqual(globalAccumulator, [24,12,4])
  })

})

