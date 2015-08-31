var parse = require('esprima').parse;
var beautify = require('js-beautify').js_beautify;
var readFileSync = require('fs').readFileSync;

var inputFile = process.argv[2];

var data = JSON.parse(readFileSync(inputFile, "utf8"));

var results = [];

for (var source in data) {
  results.push({ source: source, expected: data[source] ? parse(source) : null });
}

// JSON.stringify creates a string literal with "\u2028" or "\u2029" in it,
// which appears to choke the Rust JSON parser, so escape it.
function stringify(x) {
  return JSON.stringify(x)
             .replace(/\u2028|\u2029/g, function(m) {
               return "\\u202" + (m === "\u2028" ? "8" : "9");
             });
}

console.log(beautify(stringify(results), { indent_size: 2 }));
