.PHONY: build geometry serve

build: public/main.js

public/main.js: src/*.elm
	elm make src/Main.elm --optimize --output=public/main.js

geometry:
	Rscript data-raw/prepare_geometry.R

serve: build
	python3 -m http.server 8000 --directory public
