output/todo.js: $(wildcard *.elm)
	elm make --yes Main.elm --output ./output/todo.js

.PHONY: deploy
deploy: output/todo.js
	cp ./output/todo.js ../../static/todo.js

.PHONY: clean
clean: 
	rm -rf elm-stuff
	rm -rf output