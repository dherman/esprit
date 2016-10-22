var serialize = require('serialize-js');
var writeFileSync = require('fs').writeFileSync;

// JSON.stringify creates a string literal with "\u2028" or "\u2029" in it,
// which appears to choke the Rust JSON parser, so escape it.
function stringify(x) {
  return serialize(x, { indent: 2, forceJSON: true })
             .replace(/\u2028|\u2029/g, function(m) {
               return "\\u202" + (m === "\u2028" ? "8" : "9");
             });
}

module.exports = function(path, results) {
  writeFileSync(path, stringify(results));
};
