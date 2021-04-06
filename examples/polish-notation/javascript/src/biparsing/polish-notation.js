'use strict'
// number   : /-?[0-9]+/
// operator : '+' | '-' | '*' | '/'
// expr     : <number> | <operator> <expr>+
// polishn  : /^/ <operator> <expr>+ /$/

const {numberSimple: number} = require('./number')
const {Biparser, many, assignProperty} = require('../biparsing')
const {identity, constant} = require('../functional')

// Biparser r ((Number,Number) -> Number)
function operator() {
  this.take(1, identity)
  this.condition(x => '+-*/'.includes(x), x => `While parsing an operator a '${x}' was recieved but expected one of the operatros + - * /`)
}
exports.operator = operator
Biparser.prototype.operator = operator


// Biparser r (Either Number Node
function expr() {
  function hasOwnProperty(obj, prop) { return Object.prototype.hasOwnProperty.call(obj, prop) }
  function isNumber(x) {return hasOwnProperty(x,'mumber')}
  function isExpr(x) {return hasOwnProperty(x,'operator') && hasOwnProperty(x,'expr')}

  this.pFunction(constant({}))
  this.alternative(
    isNumber, assignProperty('number', number),
    // isExpr,   assignProperty('expr',   polishn),
    isExpr,   assignProperty('expr',   number),
  )
}
exports.expr = expr
Biparser.prototype.expr = expr

function polishn() {
  this.spaces(0)
  this.pFunction(constant({}))
  this.assignProperty('operator', operator)
  this.assignProperty('exprs', many(function() {
      this.spaces(1)
      this.expr()
    },
    identity,
  ))
}
exports.polishn = polishn
Biparser.prototype.polishn = polishn

