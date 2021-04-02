'use strict'
// number   : /-?[0-9]+/
// operator : '+' | '-' | '*' | '/'
// expr     : <number> | <operator> <expr>+
// polishn  : /^/ <operator> <expr>+ /$/

const {number, numberSimple} = require('./number')
const {Biparser} = require('../biparsing')
const {identity} = require('../functional')

// Biparser r ((Number,Number) -> Number)
function operator() {
  this.take(1, identity)
  this.condition(x => '+-*/'.includes(x), x => `While parsing an operator a '${x}' was recieved but expected one of the operatros + - * /`)
}
exports.operator = operator
Biparser.prototype.operator = operator

function hasOwnProperty(obj, prop) { return Object.prototype.hasOwnProperty.call(obj, prop) }

// Biparser r (Either Number Node
function expr() {
  this.numberSimple()
  this.pFunction(x => ({number: x}))
  // function isNumber(x) {return hasOwnProperty(x,'mumber')}
  // function isExpr(x) {return hasOwnProperty(x,'operator') && hasOwnProperty(x,'expr')}
  // function parenExpr() {
  //   this.polishn()
  //   this.referenceAll()
  // }
  // this.alternative(isNumber, number, isExpr, parenExpr)
}
exports.expr = expr
Biparser.prototype.expr = expr

function polishn() {
  this.spaces()
  this.operator()
  this.assign('operator')
  this.many(
    function() {
      this.spaces()
      this.expr()
    },
    ({exprs}) => exprs
  )
  this.assign('exprs')
  this.referenceAll()
}
exports.polishn = polishn
Biparser.prototype.polishn = polishn

