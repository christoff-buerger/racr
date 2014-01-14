#include <racr/racr.h>
#include <stdio.h>



int main(int argc, char** argv) {

	RACR_INIT(env);

	Scheme_Object* curout = scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT);
	Scheme_Object* v;

	// test
	v = racr_call("racket/base", "+", "ii", 10, 20);
	if (v) {
		scheme_display(v, curout);
		scheme_display(scheme_make_char('\n'), curout);
	}


	Scheme_Object* spec = racr_create_specification();
	racr_ast_rule(spec, "A->");
	racr_compile_ast_specifications(spec, "A");
	racr_compile_ag_specifications(spec);
	Scheme_Object* node = racr_create_ast(spec, "A", scheme_build_list(0, NULL));



	const char* t = racr_ast_node_type(node);
	printf("%s\n", t);

/*
	int bounds[][2] = {
		{ 2, 2 },
		{ 2, 4 },
		{ 3, 0 }
	};
	Scheme_Object* children = racr_ast_children(node, racr_build_bounds(3, bounds));

	int len;
	Scheme_Object** l = racr_unpack_list(list, &len);
	free(l);
	racr_create_ast(spec, "A", list);
*/


	return 0;
}



