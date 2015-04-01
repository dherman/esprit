module.exports = {
  test: function(src, expected, opts) {
    console.log(JSON.stringify({
      source: src,
      expected: expected,
      options: opts
    }) + ",");
  },
  testFail: function(src, error, opts) {
    console.log(JSON.stringify({
      source: src,
      error: error,
      options: opts
    }) + ",");
  },
  testAssert: function() {},
  misMatch: function() {}
};
