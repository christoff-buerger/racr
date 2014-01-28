#include <racr/racr.h>
#include <stdio.h>
#include <assert.h>


void exception_handler(void) {
	static int i = 1;
	printf("EXCEPTION %d\n", i++);
}


Scheme_Object* global_spec;


Scheme_Object* equation_func(int argc, Scheme_Object** argv) {
	printf("equation argc: %d\n", argc);
	return racr_create_ast(global_spec, "A", racr_list("*", *argv));
}


int main(int argc, char** argv) {


	RACR_INIT(env, "bin/bc", NULL);
	racr_set_exception_handler(exception_handler);
	//Scheme_Object* curout = scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT);



	Scheme_Object* spec = global_spec = racr_create_specification();
	racr_ast_rule(spec, "A->D");
	racr_ast_rule(spec, "B:A->");
	racr_ast_rule(spec, "C:A->A-A*-t-B-B*");
	racr_ast_rule(spec, "D->");
	racr_compile_ast_specifications(spec, "A");

	Scheme_Object* equation = scheme_make_prim_w_arity(equation_func, "equation", 1, 1);
	racr_specify_attribute_by_index(spec, "erroneous-attribute", "D", 0, 1, equation, scheme_false);

	racr_compile_ag_specifications(spec);


/*
     (assert-exception (create-ast 'UNKNOWN (list))) ; Unknown non-terminal
     (assert-exception (create-ast 'A (list))) ; Insufficient many candidates
     (assert-exception (create-ast 'A (list (create-ast-bud) 1))) ; To many candidates
     (assert-exception (create-ast 'A (list (create-ast 'A (list (create-ast-bud)))))) ; Unexpected non-list candidate
     (assert-exception (create-ast 'A (list (create-ast-list (list))))) ; Unexpected list candidate
     (assert-exception ; List does not fit because of element of wrong type
      (create-ast
       'C
       (list
        (create-ast-bud)
        (create-ast-bud)
        (create-ast-list
         (list))
        'terminal
        (create-ast-bud)
        (create-ast-list
         (list (create-ast 'A (list (create-ast-bud))))))))
     (assert-exception (create-ast-list (list 'terminal))) ; Terminal as list element
     (assert-exception (create-ast-list (list (create-ast-list (list))))) ; Nested lists
     (let ((ast (create-ast 'A (list (create-ast-bud))))) ; Candidate already part of AST
       (assert-exception (create-ast 'A (list (ast-child 'D ast))))
       (assert-exception (create-ast-list (list (ast-child 'D ast)))))
     (assert-exception (att-value 'erroneous-attribute (create-ast 'D (list))))))) ; Candidate in evaluation
*/




	Scheme_Object* ast;

	puts("TEST 1");
	racr_create_ast(spec, "UNKNOWN", racr_list(""));
	assert(racr_got_exception()); // unknown non-terminal

	puts("TEST 2");
	racr_create_ast(spec, "A", racr_list(""));
	assert(racr_got_exception()); // insufficient many candidates

	puts("TEST 3");
	racr_create_ast(spec, "A", racr_list("*i", racr_create_ast_bud(), 1));
	assert(racr_got_exception()); // too many candidates

	puts("TEST 4");
	ast = racr_create_ast(spec, "A", racr_list("*", racr_create_ast_bud()));
	racr_create_ast(spec, "A", racr_list("*", ast));
	assert(racr_got_exception()); // Unexpected non-list candidate

	puts("TEST 5");
	racr_create_ast(spec, "A", racr_list("*", racr_create_ast_list(racr_list(""))));
	assert(racr_got_exception()); // Unexpected list candidate

	puts("TEST 6");
	ast = racr_create_ast(spec, "A", racr_list("*", racr_create_ast_bud()));
	if (!racr_got_exception()) {
		racr_create_ast(spec,
			"C",
			racr_list("***s**",
				racr_create_ast_bud(),
				racr_create_ast_bud(),
				racr_create_ast_list(
					racr_list("")),
				"terminal",
				racr_create_ast_bud(),
				racr_create_ast_list(
					racr_list("*", ast))));
	}


	assert(racr_got_exception());


	return 0;
}



