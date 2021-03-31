'use strict'

// a -> Either a b
function Left(x) {
  this.isRight = false
  this.left = x
}
exports.Left = Left

// b -> Either a b
function Right(x) {
  this.isRight = true
  this.right = x
}
exports.Right = Right

