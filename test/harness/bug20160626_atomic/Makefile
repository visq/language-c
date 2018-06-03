PP=../../../examples/ParseAndPrint
TY=../../../examples/TypeCheck
all: parse
parse:
#	gcc -c -Wall test.c
	$(PP) -DHAS_FLOAT_128X test.c > test.out
	diff -u test.expect test.out
	$(TY) test.c
clean:
	rm -f *.out
gcc_check:
	gcc -fsyntax-only -c test.c
