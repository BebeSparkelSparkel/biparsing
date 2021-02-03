'use strict'

const {Reader, Writer, runWriter, execWriter} = require('./RWS')

function Serializer(toSerialize) {
  this.reader = toSerialize
  this.writer = ''
}
exports.Serializer = Serializer

// function runSerializer(func, serializer) {
//   const newSerializer = new Serializer()
//   newSerializer.reader = serializer.reader
//   newSerializer.writer = serializer.writer
//   return [func.bind(newSerializer)(), newSerializer.writer]
// }
// exports.runSerializer = runSerializer

function execSerializer(func, serializer) {
  const newSerializer = new Serializer()
  newSerializer.reader = serializer.reader
  newSerializer.writer = serializer.writer
  func.bind(newSerializer)()
  return newSerializer.writer
}
exports.execSerializer = execSerializer
Serializer.prototype = Object.assign(Serializer.prototype, Reader.prototype, Writer.prototype)

