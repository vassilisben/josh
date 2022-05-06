.PHONY: default
default:
	ocamlbuild -pkgs llvm josh.native

.PHONY: test
test:
	./josh.native < simple.josh > simple.out && cat simple.out
