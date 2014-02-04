RACKET_PATH = ../../racket/

CFLAGS = -Wall -Iinclude -I$(RACKET_PATH)/racket/include
LFLAGS = -lm -lpthread -ldl -lffi
LFLAGS += $(RACKET_PATH)/racket/src/build/racket/libracket.a
LFLAGS += $(RACKET_PATH)/racket/src/build/racket/libmzgc.a


TESTS = $(patsubst src/tests/%.c,bin/tests/%,$(wildcard src/tests/*.c))


all: $(TESTS)


build/racr.o: src/racr.c Makefile
	@mkdir -p $(@D)
	gcc $(CFLAGS) $< -c -o $@


bin/tests/%: src/tests/%.c build/racr.o Makefile
	@mkdir -p $(@D)
	gcc $(CFLAGS) build/racr.o $(LFLAGS) $< -o $@


# byte code generation
MODULES =\
petrinets/access-support\
petrinets/ast\
petrinets/composition-analysis\
petrinets/enabled-analysis\
petrinets/name-analysis\
petrinets/ui\
petrinets/well-formedness-analysis\
siple/access-support\
siple/ast\
siple/control-flow-analysis\
siple/exception-api\
siple/interpreter\
siple/lexer\
siple/name-analysis\
siple/parser\
siple/state\
siple/type\
siple/type-analysis\
siple/type-coercion\
siple/well-formedness

bin/bc: bin/make_bc Makefile
	./bin/make_bc $@ $(MODULES)

clean:
	rm -rf build
	rm -rf bin/tests

