This is the Hello World deliverable for PLT group josh.
Members: Angel Garcia, Burcu Cetin, Bora Elci, Gregory Schare, Vasileios
Benopoulos.

Our deliverable includes An example test file (point.josh) as well as a 
scanner, a parser, ast, sast and semantics checker.
The final three were largely based off of the MicroC code shown in class.

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

### Testing
```
chmod 755 run_tests.sh
./run_tests.sh
```

-------
### To build our C library (liberate_josh.bc)
```
clang -c -pthread -emit-llvm liberate_josh.c
```
