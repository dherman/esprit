var parse = require('esprima').parse;
var readFileSync = require('fs').readFileSync;
var write = require('./write');

var inputFile = process.argv[2];
var outputFile = process.argv[3];

var data = JSON.parse(readFileSync(inputFile, "utf8"));

write(outputFile, Object.keys(data).map(function(source) {
  return {
    source: source,
    expected: data[source] ? parse(source) : null
  };
}));
