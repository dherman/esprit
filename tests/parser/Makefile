INPUT = tests.in.json
OUTPUT = tests.json

$(OUTPUT): $(INPUT)
	test -d node_modules || npm install esprima
	node -e 'console.log(JSON.stringify(JSON.parse(require("fs").readFileSync("$(INPUT)","utf8")).map(function(src) { try { Function(src) } catch (e) { return { source: src, error: e.message } } try { return { source: src, expected: require("esprima").parse(src) } } catch (e) { return { source: src, error: e.description } } })))' > $(OUTPUT)
