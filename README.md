# Ark
## Build and Run the Ark compiler
```
make
```
which should output:
```
ocamlbuild -r -package llvm ark.native && \
./ark.native > codegen
```

## Compiler files
- `ast.ml`: abstract syntax tree (AST) definition
- `scanner.mll`: scanner
- `parser.mly`: parser
- `sast.mli`: definition of the semantically-checked AST
- `semantics.ml`: semantic checking
- `irgen.ml`: working Intermediate Code Generation
## Other files
- `arkTest`: testing script. Usage:
```
 ./arkTest
```

# Functionality
## Function Declarations
The entry point to your Ark program is the main function. This is how you would create a simple program that:
1. Declares a function, `foo`, that takes in three parameters and returns a string. 
2. Calls `foo` in the main function with the appropriate arguments.

If you execute this code, "hello world!" will be printed in your console!

```
foo {int a, bool b, str msg} -> str:
        return msg.
...
main {} -> int:
        printstring(foo(1, false, "hello world!")).
...
```
## Struct Declaration
Structs need to be declared and assigned seperately. Declaration can happen in 
global variable section however assignments need to happen within a function:

```
struct struct_name {
	int a.
	int b.
}.
```


## Standard Library
`print`: prints an integer
`printstring`: prints a string

## Legacy (TODO: Reformat)
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
