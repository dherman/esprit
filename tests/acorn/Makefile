all: tests.json tests-harmony.json

tests.json: tests.js
	(echo "[" && ( node tests.js | sed -e '$$s/,$$//' ) && echo "]") > tests.json

tests-harmony.json: tests-harmony.js
	(echo "[" && ( node tests-harmony.js | sed -e '$$s/,$$//' ) && echo "]") > tests-harmony.json
