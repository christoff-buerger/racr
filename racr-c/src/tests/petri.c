#include <racr/racr.h>
#include <stdio.h>
#include <error.h>

/*
	This test uses the petrinets/ui module.
	That module must be included in the bc-file.
*/


Scheme_Env* global_env;


Scheme_Object* eq_Token(int argc, Scheme_Object** argv) {
	return racr_call(scheme_eval_string("eq?", global_env), "*s", argv[0], "Token");
}
Scheme_Object* eq_Euro(int argc, Scheme_Object** argv) {
	return racr_call(scheme_eval_string("eq?", global_env), "*s", argv[0], "Euro");
}
Scheme_Object* ge_two(int argc, Scheme_Object** argv) {
	return racr_call(scheme_eval_string(">=", global_env), "*i", argv[0], 2);
}
Scheme_Object* take_any(int argc, Scheme_Object** argv) {
	return scheme_true;
}


Scheme_Object* yield_Euro(int argc, Scheme_Object** argv) {
	return racr_list("s", "Euro");
}
Scheme_Object* yield_Token(int argc, Scheme_Object** argv) {
	return racr_list("s", "Token");
}
Scheme_Object* yield_minus_two(int argc, Scheme_Object** argv) {
	long x;
	scheme_get_int_val(argv[1], &x);
	return racr_list("i", x - 2);
}
Scheme_Object* yield_yz(int argc, Scheme_Object** argv) {
	return racr_list("**", argv[1], argv[2]);
}


Scheme_Object* fire_callback(int argc, Scheme_Object** argv) {
	printf("CALLBACK - transition name: %s\n",
		SCHEME_SYM_VAL(racr_ast_child_by_name("name", argv[0])));


	return scheme_false;
}


int main(int argc, char** argv) {

	static const const char* modules[] = {
		"petrinets/main_",
		"petrinets/ui",
		NULL
	};

	RACR_INIT(env, "bin/bc", modules);
	global_env = env;


	Scheme_Object* petri_spec = scheme_eval_string("petrinet-specification", env);
	racr_ast_rule(petri_spec, "EventTransition:Transition->callback");


	racr_call(scheme_eval_string("init-ast", env), "");
	racr_call(scheme_eval_string("init-attribution", env), "");



	Scheme_Object* wrap_eq_Euro = scheme_make_prim_w_arity(eq_Euro, "eq_Euro", 1, 1);
	Scheme_Object* wrap_eq_Token = scheme_make_prim_w_arity(eq_Token, "eq_Token", 1, 1);
	Scheme_Object* wrap_ge_two = scheme_make_prim_w_arity(ge_two, "ge_two", 1, 1);
	Scheme_Object* wrap_take_any = scheme_make_prim_w_arity(take_any, "take_any", 1, 1);

	Scheme_Object* wrap_yield_Euro = scheme_make_prim_w_arity(yield_Euro, "yield_Euro", 1, -1);
	Scheme_Object* wrap_yield_Token = scheme_make_prim_w_arity(yield_Token, "yield_Token", 1, -1);
	Scheme_Object* wrap_yield_minus_two = scheme_make_prim_w_arity(yield_minus_two, "yield_minus_two", 1, -1);
	Scheme_Object* wrap_yield_yz = scheme_make_prim_w_arity(yield_yz, "yield_yz", 1, -1);

	Scheme_Object* wrap_fire_callback = scheme_make_prim_w_arity(fire_callback, "fire_callback", 1, 1);



	Scheme_Object* pn = racr_create_ast(petri_spec, "AtomicPetrinet",
		racr_list("bs***",
			0,
			"keckse",

			// places
			racr_create_ast_list(racr_list("********",
				racr_create_ast(petri_spec, "Place", racr_list("s*",
					"H",
					racr_create_ast_list(racr_list("*******",
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box*")),
						racr_create_ast(petri_spec, "Token", racr_list("s", "Box*")))))),
				racr_create_ast(petri_spec, "Place", racr_list("s*",
					"D",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Token", racr_list("s", "Token")))))),
				racr_create_ast(petri_spec, "Place", racr_list("s*",
					"G",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Token", racr_list("s", "Token")))))),
				racr_create_ast(petri_spec, "Place", racr_list("s*",
					"E",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Token", racr_list("i", 7)))))),
				racr_create_ast(petri_spec, "Place", racr_list("s*", "A", racr_create_ast_list(racr_list("")))),
				racr_create_ast(petri_spec, "Place", racr_list("s*", "F", racr_create_ast_list(racr_list("")))),
				racr_create_ast(petri_spec, "Place", racr_list("s*", "B", racr_create_ast_list(racr_list("")))),
				racr_create_ast(petri_spec, "Place", racr_list("s*", "C", racr_create_ast_list(racr_list("")))))),



			// transitions
			racr_create_ast_list(racr_list("*****",
				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"c",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "D", racr_list("*", wrap_eq_Token))))),
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "A", wrap_yield_Euro)))),
					wrap_fire_callback)),
				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"e",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "A", racr_list("*", wrap_eq_Euro))))),
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "D", wrap_yield_Token)))),
					wrap_fire_callback)),

				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"a",
					racr_create_ast_list(racr_list("***",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "A", racr_list("*", wrap_eq_Euro))),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "E", racr_list("*", wrap_ge_two))),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "G", racr_list("*", wrap_eq_Token))))),
					racr_create_ast_list(racr_list("****",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "D", wrap_yield_Token)),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "E", wrap_yield_minus_two)),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "F", wrap_yield_Euro)),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "B", wrap_yield_Token)))),
					wrap_fire_callback)),
				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"b",
					racr_create_ast_list(racr_list("**",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "B", racr_list("*", wrap_eq_Token))),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "H", racr_list("**", wrap_take_any, wrap_take_any))))),
					racr_create_ast_list(racr_list("**",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "G", wrap_yield_Token)),
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "C", wrap_yield_yz)))),
					wrap_fire_callback)),
				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"d",
					racr_create_ast_list(racr_list("*",
						racr_create_ast(petri_spec, "Arc", racr_list("s*", "C", racr_list("*", wrap_take_any))))),
					racr_create_ast_list(racr_list("")),
					wrap_fire_callback)))),


			// ports
			racr_create_ast_list(racr_list(""))));


	Scheme_Object* fire_transition = scheme_eval_string("fire-transition!", env);

	Scheme_Object* print_marking = scheme_eval_string("print-marking", env);
	Scheme_Object* print_enabled = scheme_eval_string("print-enabled", env);


	char line[256];
	for (;;) {
		racr_call(print_marking, "*", pn);
		racr_call(print_enabled, "*", pn);

		printf("Enter transition to fire:\n> ");
		fgets(line, 256, stdin);
		int l = strlen(line);
		if (l == 0) break;
		line[l - 1] = '\0';

		Scheme_Object* trans = racr_att_value("find-transition", pn, racr_list("s", line));
		if (trans == scheme_false) {
			printf("Error optaining transition\n");
			break;
		}

		printf("Firing transition...\n");
		racr_call(fire_transition, "*", trans);
		if (racr_is_ast_subtype_nt(trans, "EventTransition")) {
			racr_call(racr_ast_child_by_name("callback", trans), "*", trans);
		}
		printf("\n");
	}



	return 0;
}



