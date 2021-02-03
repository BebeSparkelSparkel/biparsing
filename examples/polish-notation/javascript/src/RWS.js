'use strict'

const {defer} = require('./functional')


function Reader(reader) {
  this.reader = reader
}
exports.Reader = Reader

// (Reader r a, r) -> a
function runReader(func, reader) {
  return func.bind(reader)()
}
exports.runReader = runReader

// Reader r r
function ask() {return this.reader}
exports.ask = ask
Reader.prototype.ask = ask

// (r' -> r, Reader r a) -> Reader r' a
function withReader(zoom, r) {
  const unzoomed = this.reader
  this.reader = zoom(unzoomed)
  const x = r.bind(this)()
  this.reader = unzoomed
  return x
}
exports.withReader = defer(withReader)
Reader.prototype.withReader = withReader

function Writer() {
  this.writer = ''
}
exports.Writer = Writer

// (WriterModifier, Writer w) -> [a, w]
function runWriter(func, writer) {
  const newWriter = new Writer(writer.writer)
  return [func.bind(newWriter)(), newWriter.writer]
}
exports.runWriter = runWriter

// (WriterModifier, Writer w) -> w
function execWriter(func, writer) {
  const [_,x] = runWriter(func, writer)
  return x
}
exports.execWriter = execWriter

// w -> Writer w
function tell(write) {
  this.writer = this.writer + write
  return undefined
}
exports.tell = defer(tell)
Writer.prototype.tell = tell

function State(state) {
  this.state = state
}
exports.State = State

function get() {return this.state}
exports.get = get
function put(x) {this.state = x; return undefined}
exports.put = defer(put)
function modify(f) {put(f(get))}
exports.modify = modify

State.prototype.get    = get
State.prototype.put    = put
State.prototype.modify = modify

// (StateModifier, State s) -> [a,s]
function runState(func, state) {
  const newState = new State(state.state)
  return [func.bind(newState)(), newState.state]
}
exports.runState = runState

// (StateModifier, State s) -> a
function evalState(func, state) {
  const [x] = runState(func, state)
  return x
}
exports.evalState = evalState

// (StateModifier, States ) -> s
function execState(func, state) {
  const [_,x] = runState(func, state)
  return x
}
exports.execState = execState

// reader writer state
function RWS(reader, state) {
  this.reader = reader
  this.writer = ''
  this.state = state
}
exports.RWS = RWS

// (RWS_Modifier, RWS) -> [a, s, w]
function runRWS(func, rws) {
  const newRWS = new RWS()
  newRWS.reader = rws.reader
  newRWS.writer = rws.writer
  newRWS.state  = rws.state
  return [func.bind(newRWS)(), newRWS.state, newRWS.writer]
}
exports.runRWS = runRWS

RWS.prototype.ask        = ask
RWS.prototype.withReader = withReader
RWS.prototype.tell       = tell
RWS.prototype.get        = get
RWS.prototype.put        = put
RWS.prototype.modify     = modify

