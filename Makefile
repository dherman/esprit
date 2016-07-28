MARKDOWN_FILES = $(wildcard doc/*.md)
HTML_FILES = $(MARKDOWN_FILES:doc/%.md=target/doc/%.html)

all: doc

publish: doc
	cd target/doc && surge --domain esprit.surge.sh

doc: api $(HTML_FILES) target/doc/rust.css

clean:
	rm -rf target/doc

api:
	cargo doc

target/doc/%.html: doc/%.md
	rustdoc --markdown-playground-url='https://play.rust-lang.org' --markdown-css rust.css $< --output=target/doc

target/doc/rust.css:
	curl -s https://raw.githubusercontent.com/rust-lang/rust/master/src/doc/rust.css > target/doc/rust.css
