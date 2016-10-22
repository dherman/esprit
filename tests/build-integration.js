var parse = require('esprima').parse;
var readFileSync = require('fs').readFileSync;
var readdirSync = require('fs').readdirSync;
var write = require('./write');

var inputDir = process.argv[2];
var outputFile = process.argv[3];

var inputFiles = readdirSync(inputDir).filter(function(fn) { return /\.js$/.test(fn) });

write(outputFile, inputFiles.map(function(filename) {
  var source = readFileSync(inputDir + "/" + filename, "utf8");
  return {
    filename: filename,
    source: source,
    expected: parse(source)
  };
}));
