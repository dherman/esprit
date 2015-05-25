var parse = require('esprima').parse;
var beautify = require('js-beautify').js_beautify;
var readFileSync = require('fs').readFileSync;

var inputFile = process.argv[2];

var data = JSON.parse(readFileSync(inputFile, "utf8"));

var results = [];

for (var source in data) {
  results.push({ source: source, expected: data[source] ? parse(source) : null });
}

console.log(beautify(JSON.stringify(results), { indent_size: 2 }));
