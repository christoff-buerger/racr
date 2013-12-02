#include <stdio.h>
#include <error.h>
#include <scheme.h>
#include <stdarg.h>
#define ARRAY_SIZE(x)	(sizeof(x) / sizeof((x)[0]))


static void declare_modules(Scheme_Env *env, const char* bc_file) {
	FILE* file = fopen(bc_file, "r");
	if (!file) error(1, 0, "coudln't read file: %s", bc_file);

	fseek(file, 0, SEEK_END);
	long size = ftell(file) - 1;
	rewind(file);

	char data[size];
	fread(data, 1, size, file);
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
};



Scheme_Env* racr_init(void* stack_addr) {

	scheme_set_stack_base(stack_addr, 1);
	Scheme_Env* env = scheme_basic_env();
	declare_modules(env, "bc");

	int i;
	for (i = 0; i < ARRAY_SIZE(module_names); i++) {
		scheme_namespace_require(scheme_intern_symbol(module_names[i]));
	}
	return env;
}




Scheme_Object* racr_call(const char* mod, const char* func, const char* fmt, ...) {

	Scheme_Object* o;
	mz_jmp_buf * volatile save, fresh;
	save = scheme_current_thread->error_buf;
	scheme_current_thread->error_buf = &fresh;

	if (!scheme_setjmp(scheme_error_buf)) {

		Scheme_Object* args[16] = {
			scheme_intern_symbol(mod),
			scheme_intern_symbol(func)
		};
		Scheme_Object* f = scheme_dynamic_require(2, args);
		int count;
		va_list ap;
		va_start(ap, fmt);

		for (count = 0; fmt[count] != '\0'; count++) {

			if (count >= 16) error(1, 0, "too many arguments");
			switch (fmt[count]) {

			case 'i':
				o = scheme_make_integer(va_arg(ap, int));
				break;

			case '*':
				o = va_arg(ap, Scheme_Object*);
				break;

			default:
				error(1, 0, "invalid format: %c", fmt[count]);
			}
			args[count] = o;
		}
		va_end(ap);
		o = scheme_apply(f, count, args);
	}
	else {
		o = NULL;
	}
	scheme_current_thread->error_buf = save;
	return o;
}





int main(int argc, char** argv) {
	int dummy;

	Scheme_Env* env = racr_init(&dummy);
	Scheme_Object* curout = scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT);


	Scheme_Object* v;

	// test
	v = racr_call("racket/base", "~", "ii", 10, 20);
	if (v) { // should fail
		scheme_display(v, curout);
		scheme_display(scheme_make_char('\n'), curout);
	}


	v = racr_call("racket/base", "+", "ii", 10, 20);
	if (v) {
		scheme_display(v, curout);
		scheme_display(scheme_make_char('\n'), curout);
	}

	return 0;
}



/*
static int run(Scheme_Env* env, int argc, char** argv) {

	declare_modules(env, "bc");

	int i;
	for (i = 0; i < ARRAY_SIZE(module_names); i++) {
		scheme_namespace_require(scheme_intern_symbol(module_names[i]));
	}

	Scheme_Object* curout = scheme_get_param(
		scheme_current_config(), MZCONFIG_OUTPUT_PORT);


	mz_jmp_buf * volatile save, fresh;
	save = scheme_current_thread->error_buf;
	scheme_current_thread->error_buf = &fresh;
	if (scheme_setjmp(scheme_error_buf)) {
		// error

	}
	else {

		FILE* file = fopen("a.scm", "r");
		fseek(file, 0, SEEK_END);
		long size = ftell(file);
		rewind(file);
		char data[size + 1];
		fread(data, 1, size, file);
		data[size] = 0;
		fclose(file);

		Scheme_Object* node;
		Scheme_Object* f;
		Scheme_Object* v;
		node = scheme_eval_string(data, env);


		if (0) {
			f = scheme_dynamic_require(2, (Scheme_Object*[]) {
				scheme_intern_symbol("racr"),
				scheme_intern_symbol("ast-node?")
			});
			v = scheme_apply(f, 1, (Scheme_Object*[]) { node });
			scheme_display(v, curout);
			scheme_display(scheme_make_char('\n'), curout);



			scheme_add_global("node", node, env);
			v = scheme_eval_string("(att-value 'state node)", env);
			scheme_display(v, curout);
			scheme_display(scheme_make_char('\n'), curout);
		}

		// access attribute
		f = scheme_dynamic_require(2, (Scheme_Object*[]) {
			scheme_intern_symbol("racr"),
			scheme_intern_symbol("att-value")
		});
		v = scheme_apply(f, 2, (Scheme_Object*[]) {
			scheme_intern_symbol("state"),
			node
		});

		scheme_display(v, curout);
		scheme_display(scheme_make_char('\n'), curout);

	}
	scheme_current_thread->error_buf = save;


//	scheme_apply(f, 2, (Scheme_Object*[]) {
//		scheme_intern_symbol("racket/base"),
//		scheme_intern_symbol("read-eval-print-loop")
//	});


	return 0;
}


int main(int argc, char** argv) {
	return scheme_main_setup(1, run, argc, argv);
}
*/
