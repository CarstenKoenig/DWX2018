.PHONY: build
build:
	stack build

.PHONY: run
run: build
	stack exec TodoServer-exe

.PHONY: clean
clean: 
	rm -rf .stack-work