.PHONY: default
default: irgen.ml
	ocamlbuild -pkgs llvm,llvm.bitreader,llvm.linker josh.native

.PHONY: test
test: josh.native simple.josh
	./josh.native < simple.josh > simple.out && lli simple.out
