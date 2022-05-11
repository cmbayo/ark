# Ark
### Build and Run the Ark compiler
```
make
```
which should output:
```
ocamlbuild -r -package llvm ark.native && \
./ark.native > codegen
```

### Compiler files
- `ast.ml`: abstract syntax tree (AST) definition
- `scanner.mll`: scanner
- `parser.mly`: parser
- `sast.mli`: definition of the semantically-checked AST
- `semantics.ml`: semantic checking
- `irgen.ml`: working Intermediate Code Generation
### Other files
- `arkTest`: testing script. Usage:
```
 ./arkTest
```
### Current working parts of the language
The compiler understands basic math (+, -, /, *). 
Compiler also takes variable assignments for ints, bools and strings.
Compiler can print ints and strings separately. 
If and else statements work. 
Currently works using main function. variables MUST be declared first. 
Example of working code:
```
main{} -> int:
	int a.
	bool b.
	str c.
	a = 2*3.
	b = true.
	c = "hello there!".
	if (b):
		print(5).
		printstring(c).
		print(a).
	... else:
		printstring("no").
	...
...
```
Example output:
```
lli codegen
5
hellothere!
6
```
