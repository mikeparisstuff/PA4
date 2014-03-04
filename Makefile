PROGRAM_NAME = main

all: $(PROGRAM_NAME)

$(PROGRAM_NAME): $(PROGRAM_NAME).ml
	ocamlc str.cma $(PROGRAM_NAME).ml

run: ${PROGRAM_NAME}
	./a.out ${f}.cl-ast

test: ${PROGRAM_NAME}
	cool --class-map --out out ${f}.cl
	cool --parse ${f}.cl
	./a.out ${f}.cl-ast
	diff -b -B -E -w out.cl-type ${f}.cl-type
