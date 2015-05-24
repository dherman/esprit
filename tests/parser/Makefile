INPUT = tests.in.json
OUTPUT = tests.json

$(OUTPUT): $(INPUT) package.json build.js
	npm install
	node build.js $(INPUT) > $(OUTPUT)
