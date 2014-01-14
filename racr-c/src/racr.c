#include <racr/racr.h>
#include <error.h>


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
	NULL
};



Scheme_Env* racr_init(void* stack_addr) {
	scheme_set_stack_base(stack_addr, 1);
	Scheme_Env* env = scheme_basic_env();

	declare_modules(env, "bc");

	const char** mods = module_names;
	for (; *mods; mods++) scheme_namespace_require(scheme_intern_symbol(*mods));

	return env;
}



// racr helper functions

Scheme_Object* racr_call(const char* mod, const char* func, const char* fmt, ...) {

	Scheme_Object* o;
	Scheme_Object* rest = NULL;
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

			case 's':
				o = scheme_intern_symbol(va_arg(ap, char*));
				break;

			case 'b':
				o = va_arg(ap, int) ? scheme_true : scheme_false;
				break;

			case '*':
				o = va_arg(ap, Scheme_Object*);
				break;

			case '.':
				if (fmt[count + 1] != '\0') {
					error(1, 0, "invalid format string: %s", fmt);
				}
				rest = va_arg(ap, Scheme_Object*);
				break;

			default:
				error(1, 0, "invalid format string: %s", fmt);
			}
			args[count] = o;
		}
		va_end(ap);

		if (!rest) o = scheme_apply(f, count, args);
		else {
			Scheme_Object* l = scheme_append(scheme_build_list(count - 1, args), rest);
			scheme_apply_to_list(f, l);
		}
	}
	else {
		o = NULL;
	}
	scheme_current_thread->error_buf = save;
	return o;
}

Scheme_Object* racr_build_bounds(int len, int bounds[][2]) {
	Scheme_Object* b[len];
	int i;
	for (i = 0; i < len; i++) {
		b[i] = scheme_make_pair(
			scheme_make_integer(bounds[i][0]),
			(bounds[i][1] <= 0)
				? scheme_make_integer(bounds[i][1])
				: scheme_intern_symbol("*"));
	}
	return scheme_build_list(len, b);
}






Scheme_Object* racr_create_specification(void) {
	return racr_call("racr", "create-specification", "");
}

// Abstract Syntax Trees
void racr_ast_rule(Scheme_Object* spec, const char* symbol_encoding_rule) {
	racr_call("racr", "ast-rule", "*s", spec, symbol_encoding_rule);
}
void racr_compile_ast_specifications(Scheme_Object* spec, const char* start_symbol) {
	racr_call("racr", "compile-ast-specifications", "*s", spec, start_symbol);
}
int racr_is_ast_node(Scheme_Object* scheme_entity) {
	return SCHEME_TRUEP(racr_call("racr", "ast-node?", "*", scheme_entity));
}
Scheme_Object* racr_create_ast(
		Scheme_Object* spec,
		const char* non_terminal,
		Scheme_Object* list_of_children) {
	return racr_call("racr", "create-ast", "*s*",
		spec,
		non_terminal,
		list_of_children);
}
Scheme_Object* racr_create_ast_list(Scheme_Object* list_of_children) {
	return racr_call("racr", "create-ast-list", "*", list_of_children);
}
Scheme_Object* racr_create_ast_bud(void) {
	return racr_call("racr", "create-ast-bud", "");
}
Scheme_Object* racr_ast_parent(Scheme_Object* n) {
	return racr_call("racr", "ast-parent", "*", n);
}
Scheme_Object* racr_ast_child_by_index(Scheme_Object* n, int index) {
	return racr_call("racr", "ast-child", "*i", n, index);
}
Scheme_Object* racr_ast_child_by_name(Scheme_Object* n, const char* name) {
	return racr_call("racr", "ast-child", "*s", n, name);
}
Scheme_Object* racr_ast_sibling_by_index(Scheme_Object* n, int index) {
	return racr_call("racr", "ast-sibling", "*i", n, index);
}
Scheme_Object* racr_ast_sibling_by_name(Scheme_Object* n, const char* name) {
	return racr_call("racr", "ast-sibling", "*s", n, name);
}
/*
// TODO: define-syntax
Scheme_Object* racr_ast_children(Scheme_Object* n, Scheme_Object* b) {
	return racr_call("racr", "ast-children", "*.", n, b);
}
void racr_ast_for_each_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	racr_call("racr", "ast-for-each-child", "**.", f, n, b);
}
Scheme_Object* racr_ast_find_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	return racr_call("racr", "ast-find-child", "**.", f, n, b);
}
Scheme_Object* racr_ast_find_child_(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	return racr_call("racr", "ast-find-child*", "**.", f, n, b);
}
// TODO: missing functions
int racr_ast_has_parent(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call("racr", "ast-has-parent?", "*", n));
}
int racr_ast_has_child(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call("racr", "ast-has-child?", "*", n));
}
int racr_ast_has_sibling(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call("racr", "ast-has-sibling?", "*", n));
}
*/
int racr_ast_child_index(Scheme_Object* n) {
	return SCHEME_INT_VAL(racr_call("racr", "ast-child-index", "*", n));
}
int racr_ast_num_children(Scheme_Object* n) {
	return SCHEME_INT_VAL(racr_call("racr", "ast-num-children", "*", n));
}
const char* racr_ast_node_type(Scheme_Object* n) {
	return SCHEME_SYM_VAL(racr_call("racr", "ast-node-type", "*", n));
}
int racr_is_ast_list_node(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call("racr", "ast-list-node?", "*", n));
}
int racr_is_ast_bud_node(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call("racr", "ast-bud-node?", "*", n));
}
int racr_is_ast_subtype_nn(Scheme_Object* a1, Scheme_Object* a2) {
	return SCHEME_TRUEP(racr_call("racr", "ast-subtype?", "**", a1, a2));
}
int racr_is_ast_subtype_nt(Scheme_Object* a, const char* t) {
	return SCHEME_TRUEP(racr_call("racr", "ast-subtype?", "*s", a, t));
}
int racr_is_ast_subtype_tn(const char* t, Scheme_Object* a) {
	return SCHEME_TRUEP(racr_call("racr", "ast-subtype?", "s*", t, a));
}

// Attribution
void racr_specify_attribute_by_index(
		Scheme_Object* spec,
		const char* att_name,
		const char* non_terminal,
		int index,
		int is_cached,
		Scheme_Object* equation,
		Scheme_Object* circ_def) {
	racr_call("racr", "specify-attribute", "*ssib**",
		spec,
		att_name,
		non_terminal,
		index,
		is_cached,
		equation,
		circ_def);
}
void racr_specify_attribute_by_name(
		Scheme_Object* spec,
		const char* att_name,
		const char* non_terminal,
		const char* name,
		int is_cached,
		Scheme_Object* equation,
		Scheme_Object* circ_def) {
	racr_call("racr", "specify-attribute", "*ss*b**",
		spec,
		att_name,
		non_terminal,
		name,
		is_cached,
		equation,
		circ_def);
}
void racr_compile_ag_specifications(Scheme_Object* spec) {
	racr_call("racr", "compile-ag-specifications", "*", spec);
}
Scheme_Object* racr_att_value(
		Scheme_Object* spec,
		const char* attribute_name,
		Scheme_Object* node,
		Scheme_Object* arguments) {
	return racr_call("racr", "att-value", "*s*.",
		spec,
		attribute_name,
		node,
		arguments);
}

// Rewriting
Scheme_Object* racr_rewrite_terminal(int i, Scheme_Object* n, Scheme_Object* new_value) {
	return racr_call("racr", "rewrite-terminal", "i**", i, n, new_value);
}
void racr_rewrite_refine(Scheme_Object* n, const char* t, Scheme_Object* c) {
	racr_call("racr", "rewrite-refine", "*s.", n, t, c);
}
Scheme_Object* racr_rewrite_abstract(Scheme_Object* n, const char* t) {
	return racr_call("racr", "rewrite-abstract", "*s", n, t);
}
Scheme_Object* racr_rewrite_subtree(
		Scheme_Object* old_fragment,
		Scheme_Object* new_fragment) {
	return racr_call("racr", "rewrite-subtree", "**", old_fragment, new_fragment);
}
void racr_rewrite_add(Scheme_Object* l, Scheme_Object* e) {
	racr_call("racr", "rewrite-add", "**", l, e);
}
void racr_rewrite_insert(Scheme_Object* l, int i, Scheme_Object* e) {
	racr_call("racr", "rewrite-insert", "*i*", l, i, e);
}
Scheme_Object* racr_rewrite_delete(Scheme_Object* n) {
	return racr_call("racr", "rewrite-delete", "*", n);
}
Scheme_Object* racr_perform_rewrites(
		Scheme_Object* n,
		Scheme_Object* strategy,
		Scheme_Object* transformers) {
	return racr_call("racr", "perform-rewrites", "**.", n, strategy, transformers);
}

// AST Annotations
void racr_ast_annotation_set(Scheme_Object* n, const char* a, Scheme_Object* v) {
	racr_call("racr", "ast-annotation-set!", "*s*", n, a, v);
}
void racr_ast_weave_annotations(Scheme_Object* n, const char* t, const char* a, Scheme_Object* v) {
	racr_call("racr", "ast-weave-annotations", "*ss*", n, t, a, v);
}
void racr_ast_annotation_remove(Scheme_Object* n, const char* a) {
	racr_call("racr", "ast-annotation-remove!", "*s", n, a);
}
int racr_is_ast_annotation(Scheme_Object* n, const char* a) {
	return SCHEME_TRUEP(racr_call("racr", "ast-annotation?", "*s", n, a));
}
Scheme_Object* racr_ast_annotation(Scheme_Object* n, const char* a) {
	return racr_call("racr", "ast-annotation", "*s", n, a);
}



