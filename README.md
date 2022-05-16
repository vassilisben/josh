PLT Group Josh
Members: Angel Garcia, Burcu Cetin, Bora Elci, Gregory Schare, Vasileios Benopoulos.

Our deliverable includes the scanner, parser, ast, sast, semantics checker, and irgen.

### Build the standard library
```
clang -c -pthread -emit-llvm liberate_josh.c
```

### Build the Josh compiler

```
ocamlbuild -pkgs llvm,llvm.bitreader,llvm.linker josh.native
```

### Run the Josh compiler and generate llvm code
```
./josh.native -l simple.josh > simple.out
```

### Run the llvm code
```
lli simple.out
```

-------
### Testing
```
./run_tests.sh
```

### Run the interactive Banking Example
```
./josh.native < examples/banking.josh > banking.out && lli banking.out
```
