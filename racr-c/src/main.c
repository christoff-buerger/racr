#include <stdio.h>
#include <error.h>
#include <racket/scheme.h>
#define ARRAY_SIZE(x)	(sizeof(x) / sizeof((x)[0]))


static void declare_modules(Scheme_Env *env, const char* bc_file) {
	FILE* file = fopen(bc_file, "r");
	if (!file) error(1, 0, "coudln't read file: %s", bc_file);

	fseek(file, 0, SEEK_END);
	long size = ftell(file) - 1;
	rewind(file);

	char data[size];
	fread(data, 1, size, file);
	data[size] = 0;
	fclose(file);

	scheme_register_embedded_load(size, data);
	scheme_embedded_load(size, data, 1);
}


static const char* module_names[] = {
	"racket/base",
	"racr",

	"petrinets/access-support",
	"petrinets/ast",
	"petrinets/composition-analysis",
	"petrinets/enabled-analysis",
	"petrinets/name-analysis",
	"petrinets/ui",
	"petrinets/well-formedness-analysis",
/*
	"siple/access-support",
	"siple/ast",
	"siple/control-flow-analysis",
	"siple/exception-api",
	"siple/interpreter",
	"siple/lexer",
	"siple/name-analysis",
	"siple/parser",
	"siple/state",
	"siple/type",
	"siple/type-analysis",
	"siple/type-coercion",
	"siple/well-formedness",
*/
};


static int run(Scheme_Env* env, int argc, char** argv) {

	printf("%p\n", scheme_current_thread);

	declare_modules(env, "bc");

//	scheme_namespace_require(scheme_intern_symbol("racket/base"));
//	scheme_namespace_require(scheme_intern_symbol("racr"));
	for (int i = 0; i < ARRAY_SIZE(module_names); i++) {
		scheme_namespace_require(scheme_intern_symbol(module_names[i]));
	}

	Scheme_Object* curout = scheme_get_param(
		scheme_current_config(), MZCONFIG_OUTPUT_PORT);



	mz_jmp_buf * volatile save, fresh;
	save = scheme_current_thread->error_buf;
	scheme_current_thread->error_buf = &fresh;
	if (scheme_setjmp(scheme_error_buf)) {

	}
	else {

		Scheme_Object* q[2] = {
			scheme_intern_symbol("racr"),
			scheme_intern_symbol("ast-node?")
		};
		Scheme_Object* n = scheme_dynamic_require(2, q);
		n = scheme_apply(n, 0, NULL);

		scheme_display(n, curout);
		scheme_display(scheme_make_char('\n'), curout);


	}
	scheme_current_thread->error_buf = save;





/*
	// add global to environment
	scheme_add_global("a", scheme_make_integer(100), env);
*/


	Scheme_Object* a[2] = {
		scheme_intern_symbol("racket/base"),
		scheme_intern_symbol("read-eval-print-loop")
	};
	scheme_apply(scheme_dynamic_require(2, a), 0, NULL);

	return 0;
}


int main(int argc, char** argv) {
	return scheme_main_setup(1, run, argc, argv);
}
