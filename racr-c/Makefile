LDFLAGS=-lm -lpthread -ldl -lffi\
	../../racket/racket/src/build/racket/libracket.a\
	../../racket/racket/src/build/racket/libmzgc.a

MODULES=\
racr\
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

SRC=src/main.c
OBJ=$(SRC:src/%.c=build/%.o)

a.out: $(OBJ) Makefile
	c99 $< $(LDFLAGS)

build/%.o: src/%.c Makefile
	@mkdir -p $(@D)
	c99 -Wall -c $< -o $@


# byte code generation
bc: src/make_bc Makefile
	./src/make_bc bc $(MODULES)

clean:
	rm -rf build
	rm -rf a.out

