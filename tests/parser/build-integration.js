var parse = require('esprima').parse;
var beautify = require('js-beautify').js_beautify;
var readFileSync = require('fs').readFileSync;
var readdirSync = require('fs').readdirSync;

var inputDir = process.argv[2];

var inputFiles = readdirSync(inputDir).filter(function(fn) { return /\.js$/.test(fn) });

var results = [];

inputFiles.forEach(function(filename) {
  var source = readFileSync(inputDir + "/" + filename, "utf8");
  results.push({
    filename: filename,
    source: source,
    expected: parse(source)
  });
});

// JSON.stringify creates a string literal with "\u2028" or "\u2029" in it,
// which appears to choke the Rust JSON parser, so escape it.
function stringify(x) {
  return JSON.stringify(x)
             .replace(/\u2028|\u2029/g, function(m) {
               return "\\u202" + (m === "\u2028" ? "8" : "9");
             });
}

// FIXME: remove the .slice(3, 4) when we're done testing underscore
console.log(beautify(stringify(results/*.slice(3,4)*/), { indent_size: 2 }));
