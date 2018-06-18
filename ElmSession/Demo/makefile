.PHONY: haskell
haskell: elm
	$(MAKE) -C server/Haskell run

.PHONY: fsharp
fsharp: elm
	$(MAKE) -C server/Fsharp run

.PHONY: csharp
csharp: elm
	$(MAKE) -C server/Csharp run


elm:
	$(MAKE) -C client/Elm deploy
	

.PHONY: deploy
deploy: output/todo.js
	cp ./output/todo.js ../../static/todo.js

.PHONY: clean
clean: 
	$(MAKE) -C client/Elm clean
	$(MAKE) -C server/Haskell clean