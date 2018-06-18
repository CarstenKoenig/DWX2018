.PHONY: build
build:
	elm make --yes src/Main.elm --output ./output/todo.js

deploy: build
	cp ./output/todo.js ../../static/todo.js

.PHONY: live
live:
	elm-live src/Main.elm --output=../../static/todo.js --dir=../../static/ --open --pushstate


.PHONY: clean
clean: 
	rm -rf elm-stuff
	rm -rf output