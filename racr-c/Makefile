C = -Wall -c -O2 -fPIC -I../../racket/racket/include
L = -lm -lpthread -ldl -lffi\
	../../racket/racket/src/build/racket/libracket.a\
	../../racket/racket/src/build/racket/libmzgc.a
SRC = src/main.c
OBJ = $(SRC:src/%.c=build/%.o)
TRG = a.out

$(TRG): $(OBJ) Makefile
	gcc $< $(L) -o $@


build/%.o: src/%.c Makefile
	@mkdir -p $(@D)
	gcc -Wall $(C) $< -o $@


# byte code generation
MODULES =\
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

bc: src/make_bc Makefile
	./src/make_bc bc $(MODULES)

clean:
	rm -rf build
	rm -rf a.out

