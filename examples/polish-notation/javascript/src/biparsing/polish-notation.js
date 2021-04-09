'use strict'
// number   : /-?[0-9]+/
// operator : '+' | '-' | '*' | '/'
// expr     : <number> | <operator> <expr>+
// polishn  : /^/ <operator> <expr>+ /$/

const {numberSimple: number} = require('./number')
const {Biparser, atLeast1, assignProperty, ParseError} = require('../biparsing')
const {identity, constant, defer} = require('../functional')
const {recurse} = require('./recurse')

// Biparser r ((Number,Number) -> Number)
function operator() {
  this.take(1, identity)
  this.condition(x => '+-*/'.includes(x), x => `While parsing an operator a '${x}' was recieved but expected one of the operatros + - * /`)
}
exports.operator = operator
Biparser.prototype.operator = operator


// Biparser r (Either Number Node
const expr = recurse(function exprBiparser() {
  const args = arguments

  this.newNamespace()
  this.alternativeOrError(
    hasOwnProperty('number'), assignProperty('number', number),
    hasOwnProperty('expr'),   assignProperty('expr',   defer(polishn)(...args)),
    new ParseError('could not match number nor expr'),
  )
})
exports.expr = expr
Biparser.prototype.expr = expr

const polishn = recurse(function polishnBiparser() {
  const args = arguments

  this.spaces(0)
  this.newNamespace()
  this.assignProperty('operator', operator)
  this.assignProperty('exprs', atLeast1(function() {
      this.spaces(1)
      this.expr(...args)
    },
    identity,
  ))
})
exports.polishn = polishn
Biparser.prototype.polishn = polishn

function hasOwnProperty(prop) { return function(obj) { return Object.prototype.hasOwnProperty.call(obj, prop) } }

