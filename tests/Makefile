UNIT_IN = unit.in.json
UNIT_OUT = build/unit.json

INTEGRATION_OUT = build/integration.json

all: $(INTEGRATION_OUT) $(UNIT_OUT)
.PHONY: all

node_modules/.exists: package.json
	npm install
	touch $@

$(INTEGRATION_OUT): $(wildcard integration/*.js) node_modules/.exists build-integration.js
	node build-integration.js integration $(INTEGRATION_OUT)

$(UNIT_OUT): $(UNIT_IN) node_modules/.exists build-unit.js
	node build-unit.js $(UNIT_IN) $(UNIT_OUT)
