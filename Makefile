.PHONY: default
default: irgen.ml
	ocamlbuild -pkgs llvm,llvm.bitreader,llvm.linker josh.native

SRC=$(wildcard *.josh)
TAR = $(SRC:.josh=.out)

%.josh: default

%.out: %.josh
	./josh.native -l $< > $@ && lli $@
	touch $<

.PHONY: all
all: $(TAR)

.PHONY: clean
clean:
	rm -rf _build/ $(TAR) *.native
