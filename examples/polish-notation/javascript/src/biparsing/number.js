'use strict'

const {Just, Nothing, fromMaybe, maybe} = require('../Maybe')
const {identity, compose, constant, foldl, foldr, unfold} = require('../functional')
const {Biparser, string, optional} = require('../biparsing')


// uses built in parsing and serializing javascript functions (not proud of this parser)
function numberSimple() {
  this.takeWhile(
    x => '-.0123456789'.includes(x), // return true if the character should be in a number. no ordering accounted for so an ill formed number charcaters would still be parsed
    x => x.toString(), // uses the built in javascript number to string serializer to convert the number to a string for serialization
  )
  this.pFunction(Number.parseFloat) // uses the built in javascript Number.parseFloat to parse the string of charaters from takeWhile into a number
  this.condition(x => !Number.isNaN(x), constant('Expected a number not NaN')) // throws a parse error if the parsed number is NaN
}
exports.numberSimple = numberSimple
Biparser.prototype.numberSimple = numberSimple

// Biparser r Digit
function digit() {
  this.take(1, identity) // take one char while parsing and write one char when serializing
  this.condition(
    x => '0123456789'.includes(x), // check if character is a digit
    x => `digit expected a 0123456789 but received ${x}`, // constructs the ParseError if the character is not a digit
  )
}
exports.digit = digit
Biparser.prototype.digit = digit

function number() {
  // sign section
  // biparses the negative sign or lack of positive sign
  this.optional(
    x => x < 0, // while serializing if the value is less than 0, run the serializaion function of `string('-')`
    string('-'), // biparses the negative symbol '-'
  )
  this.pFunction(fromMaybe('')) // if no negative sign is parsed return an empty string for the sign
  this.assign('sign') // assign to parsing variable 'sign' the value '-' or '' that was produced by the sign section

  // whole number section
  // biparses the digits between the sign and the decimal point
  function getWholePart(x) {return Math.floor(Math.abs(x))} // function to isolate the whole positive part of the number. Basically, removes negative sign, decimal point, and fractional (right of decimal point) digits
  this.manyN(
    1, // at least one digit needs to be parsed
    digit, // digit biparser. While parsing digit will be run by manN until it fails to parse a digit character. It will usually fail on the decimal point. While serializing manyN will pass a digit character to digit and digit will write that to the output string.
    x => Array.from(getWholePart(x).toString()), // converts the whole part of the number to an array of digit charaters that will be individually passed to the serializing part of digit
  )
  function arrayDigits2stringDigits(digits) {return digits.reduce((x,y) => x + y)} // converts the array of digits that the parsing part of many and manyN produces into a string of the digits. Ex. ['1','2','3'] is converted to '123'
  this.pFunction(arrayDigits2stringDigits) // combines all the digts that the parsing part of manyN and digit produced into a string
  this.assign('wholeDigits') // assigns the variable wholeDigits the string of digits parsed in the whole number section

  // fractional number section
  // biparses the decimal point and fractional digits
  function getFractionalPart(x) {return Math.abs(x) - getWholePart(x)} // function to isolate the fractional part of the number. Basically, removes the negative sign and the whole digits.
  this.optional(
    x => getFractionalPart(x) > 0, // serialize the fractional part if it exists
    function() { // fractional digit serializer
      this.string('.') // because biparser is in optional, this.string('.') tries to biparse the decimal point. While parsing if this.string('.') fails optional exits and returns the Nothing object. While serializing this will only be run if the first function argument to optional returns true, otherwise optional does not run the serialization.
      this.many( // attempts to get many digits after the decimal point
        digit,
        x => Array.from(getFractionalPart(x).toString().slice(2)), // isolates the fractional part of the number and breaks it up into individual digit charaters for serialization
      )
      this.pFunction(arrayDigits2stringDigits)
    },
  )
  this.pFunction(fromMaybe('')) // if no fractional characters were matched while parsing return an empty string as the parse result
  this.assign('fractionalDigits') // assign to the parse variable fractionalDigits the string that was parsed in the fractional number section

  // transforms number string into number
  this.referenceAll() // in the parser, returns all the assigned variables as an object
  this.pFunction(
    function({sign,wholeDigits,fractionalDigits}) { // unpack the parser variables object
      const numberString = sign + wholeDigits + '.' + fractionalDigits // concatenate the parsed number string parts
      return Number.parseFloat(numberString) // use javascript native number parser to convert the number string to a number
      // UNSATISFIED. If you are unsatisfied with using the build in javascript number parser see the biparser numberZoom that uses folds to create the number from the digits
    })
}
exports.number = number
Biparser.prototype.number = number

function numberZoom() { this.newAssignmentSpace(function() {
  this.optional(x => x < 0, string('-'))
  this.pFunction(maybe(1,constant(-1)))
  this.assign('sign')
  this.zoom(Math.abs, function() {
    this.zoom(Math.floor, function() {
      this.manyN(1, digit, x => Array.from(x.toString())) // should probably use unfold for serialization for demonstration purposes
      this.pFunction(foldl((x,y) => x * 10 + Number.parseInt(y))(0))
      this.assign('whole')
    })
    this.zoom(
      x => {const y = Math.abs(x); return y - Math.floor(y)},
      optional(x => x > 0, function() {
        this.string('.')
        this.manyN(1, digit, unfold(compose(
          x => {
            const y = Math.floor(x)
            const z = x - y
            return [y.toString(), z > 0 ? new Just(z) : Nothing]
          },
          x => x * 10,
        )))
        this.pFunction(compose(
          x => x / 10,
          foldr((x,y) => Number.parseInt(x) + y / 10)(0),
        ))
      }),
    )
    this.pFunction(fromMaybe(0))
    this.assign('fractional')
    this.referenceAll()
    this.pFunction(({sign,whole,fractional}) => sign * (whole + fractional))
  })
})}
exports.numberZoom = numberZoom
Biparser.prototype.numberZoom = numberZoom

