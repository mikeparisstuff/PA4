PROGRAM_NAME = main

all: $(PROGRAM_NAME)

$(PROGRAM_NAME): $(PROGRAM_NAME).ml
	ocamlc $(PROGRAM_NAME).ml

run: ${PROGRAM_NAME}
	./a.out ${f}

test: ${PROGRAM_NAME}
	./a.out ${f}
	diff -b -B -E -w out.cl-ast ${f}