UNIT_IN = unit.in.json
UNIT_OUT = build/unit.json

INTEGRATION_OUT = build/integration.json

all: $(INTEGRATION_OUT) $(UNIT_OUT)
.PHONY: all

$(INTEGRATION_OUT): $(wildcard integration/*.js) package.json build-integration.js
	npm install
	node build-integration.js integration/ > $(INTEGRATION_OUT)

$(UNIT_OUT): $(UNIT_IN) package.json build-unit.js
	npm install
	node build-unit.js $(UNIT_IN) > $(UNIT_OUT)
