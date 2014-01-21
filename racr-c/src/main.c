#include <racr/racr.h>
#include <stdio.h>




int main(int argc, char** argv) {

	static const char* mods[] = {
/*
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
*/
		NULL
	};

	RACR_INIT(env, "bc", mods);


	// test racr_ast_children()
	Scheme_Object* spec = racr_create_specification();
	racr_ast_rule(spec, "S->t1-t2-t3-t4-t5");
	racr_compile_ast_specifications(spec, "S");
	racr_compile_ag_specifications(spec);

	Scheme_Object* list = racr_call("rnrs", "list", "iiiii", 10, 20, 30, 40, 50);
	Scheme_Object* ast = racr_create_ast(spec, "S", list);

	int bounds[][2] = {
		{ 2, 3 },
		{ 2, 4 },
		{ 3, 0 }
	};

	Scheme_Object* c = racr_ast_children(ast, racr_build_bounds(3, bounds));


	Scheme_Object* curout = scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT);
	scheme_display(c, curout);
	scheme_display(scheme_make_char('\n'), curout);


	Scheme_Object* d = racr_ast_children(ast, scheme_build_list(0, NULL));
	scheme_display(d, curout);
	scheme_display(scheme_make_char('\n'), curout);


	scheme_display(racr_ast_child_by_name("t3", ast), curout);
	scheme_display(scheme_make_char('\n'), curout);

	return 0;
}



