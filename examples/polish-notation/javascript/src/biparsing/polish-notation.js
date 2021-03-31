// number   : /-?[0-9]+/
// operator : '+' | '-' | '*' | '/'
// expr     : <number> | '(' <operator> <expr>+ ')'
// polishn  : /^/ <operator> <expr>+ /$/

const {number} = require('./number')


// (a, [Node a]) -> Node a
function Node(x, children) {
  this.x = x
  this.children = children
}

// a -> Node a
function Leef(x) {return new Node(x,[])}

// Biparser r ((Number,Number) -> Number)
function operator() {
  this.take(1, x => 
  this.condition(x => '+-*/'.includes(x), x => `While parsing an operator a '${x}' was recieved but expected one of the operatros + - * /`)
}
function add(x,y) {return x + y}
function subtract(x,y) {return x - y}
function multiply(x,y) {return x * y}
function divide(x,y) {return x / y}

// Biparser r (Either Number Node 
function expr() {
}

function polishn() {
}
