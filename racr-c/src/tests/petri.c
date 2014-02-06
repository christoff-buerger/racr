#include <racr/racr.h>
#include <stdio.h>
#include <error.h>



Scheme_Env* global_env;



Scheme_Object* fire_callback(int argc, Scheme_Object** argv) {
	printf("CALLBACK - transition name: %s\n",
		SCHEME_SYM_VAL(racr_ast_child_by_name("name", argv[0])));

	return scheme_false;
}



Scheme_Object* eq_Token(int argc, Scheme_Object** argv) {
	return racr_call(scheme_eval_string("eq?", global_env), "*s", argv[0], "Token");
}
Scheme_Object* eq_Euro(int argc, Scheme_Object** argv) {
	return racr_call(scheme_eval_string("eq?", global_env), "*s", argv[0], "Euro");
}

Scheme_Object* yield_Euro(int argc, Scheme_Object** argv) {
	return racr_list("s", "Euro");
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
			racr_create_ast_list(racr_list("*",
				racr_create_ast(petri_spec, "EventTransition", racr_list("s***",
					"c",
					racr_create_ast_list(racr_list("*", racr_create_ast(petri_spec, "Arc", racr_list("s*",
						"D", racr_list("*", scheme_make_prim_w_arity(eq_Token, "eq_Token", 1, 1)))))),
					racr_create_ast_list(racr_list("*", racr_create_ast(petri_spec, "Arc", racr_list("s*",
						"A", scheme_make_prim_w_arity(yield_Euro, "yield_Euro", 1, -1))))),
					scheme_make_prim_w_arity(fire_callback, "callback", 1, 1))))),


			// ports
			racr_create_ast_list(racr_list(""))));


	Scheme_Object* fire_transition = scheme_eval_string("fire-transition!", env);

	Scheme_Object* print_marking = scheme_eval_string("print-marking", env);
	Scheme_Object* print_enabled = scheme_eval_string("print-enabled", env);

	racr_call(print_marking, "*", pn);
	racr_call(print_enabled, "*", pn);


	printf("\n");
	printf("Firing transition c.\n");
	Scheme_Object* trans = racr_att_value("find-transition", pn, racr_list("s", "c"));
	racr_call(fire_transition, "*", trans);
	if (racr_is_ast_subtype_nt(trans, "EventTransition")) {
		racr_call(racr_ast_child_by_name("callback", trans), "*", trans);
	}
	printf("Transition fired.\n");
	printf("\n");


	racr_call(print_marking, "*", pn);
	racr_call(print_enabled, "*", pn);



	return 0;
}



