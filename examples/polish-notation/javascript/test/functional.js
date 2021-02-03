'use strict'
const assert = require('assert')

const {compose, foldl, foldr, defer} = require('../src/functional.js')


describe('functional', function() {
  it('compose', function() { assert.equal(
    compose(x => x / 10, x => x / 2)(100),
    5
  )})

  it('foldl', function() { assert.equal(
    foldl((x,y) => x / y, 100, [10,2]),
    5
  )})

  it('foldr', function() { assert.equal(
    foldr((x,y) => x / y, 2, [100, 10]),
    20
  )})

  it('defer', function() { assert.equal(
    defer((x,y,z) => x * y * z)(2,3,4)(),
    24
  )})

})

