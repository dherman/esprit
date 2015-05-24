var parse = require('esprima').parse;
var beautify = require('js-beautify').js_beautify;
var readFileSync = require('fs').readFileSync;

var inputFile = process.argv[2];

var data = JSON.parse(readFileSync(inputFile, "utf8")).map(function(source) {
  // Esprima sometimes fails to reject invalid JS programs, so give
  // V8 a chance to syntax check it too. Wrap the body in a function
  // we don't call in case it has side effects or infinite loops.
  try {
    Function(source);
  } catch (e) {
    return {
      source: source,
      error: e.message
    };
  }

  try {
    // Esprima says it's a successful parse.
    return {
      source: source,
      expected: parse(source)
    };
  } catch (e) {
    // Esprima says it's a parse error.
    return {
      source: source,
      error: e.description
    };
  }
});

console.log(beautify(JSON.stringify(data), { indent_size: 2 }));
