Complete implementation of a JavaScript biparser for Polish Notation to its AST and back again.

To see the Polish Notation biparser code see [src/biparsing/polish-notation.js](./javascript/src/biparsing/polish-notation.js).

```javascript
const {polishn} = require('./src/biparsing/polish-notation')
const {genParserSerializer, evalParser, runParser, Parser, execSerializer, Serializer} = require('./src/biparsing')

const {parser, serializer} = genParserSerializer(biparser)

const toParse = '/ 2 + 6 5'
const parseResult = evalParser(parser, new Parser(toParse))
console.log('parseResult', parseResult)


const toSerialize = {operator: '/', exprs: [{number: 2}, {expr: {operator: '+', exprs: [{number: 6}, {number: 5}]}}]}
const serializeResult = execSerializer(serializer, new Serializer(toSerialize))
console.log('serializeResult', serializeResult)
```

There are more examples of running the parser and serializer in [test/biparsing/polish-notation.js](./javascript/test/biparsing/polish-notation.js).
