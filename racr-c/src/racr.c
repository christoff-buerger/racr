#include <racr/racr.h>
#include <error.h>



static const char* racr_function_strings[] = {
	"list",
	"append",
	"apply",
	"cons",

	"create-specification",
	"ast-rule",
	"compile-ast-specifications",
	"ast-node?",
	"create-ast",
	"create-ast-list",
	"create-ast-bud",
	"ast-parent",
	"ast-child",
	"ast-sibling",
	"ast-children",
	"ast-for-each-child",
	"ast-find-child",
	"ast-find-child*",
	"ast-has-parent?",
	"ast-has-child?",
	"ast-has-sibling?",
	"ast-child-index",
	"ast-num-children",
	"ast-node-type",
	"ast-list-node?",
	"ast-bud-node?",
	"ast-subtype?",
	"specify-attribute",
	"compile-ag-specifications",
	"att-value",
	"rewrite-terminal",
	"rewrite-refine",
	"rewrite-abstract",
	"rewrite-subtree",
	"rewrite-add",
	"rewrite-insert",
	"rewrite-delete",
	"perform-rewrites",
	"ast-annotation-set!",
	"ast-weave-annotations",
	"ast-annotation-remove!",
	"ast-annotation?",
	"ast-annotation"
};


static union {
	Scheme_Object* array[0];
	struct {
		Scheme_Object* list;
		Scheme_Object* append;
		Scheme_Object* apply;
		Scheme_Object* cons;

		Scheme_Object* create_specification;
		Scheme_Object* ast_rule;
		Scheme_Object* compile_ast_specifications;
		Scheme_Object* ast_node_q;
		Scheme_Object* create_ast;
		Scheme_Object* create_ast_list;
		Scheme_Object* create_ast_bud;
		Scheme_Object* ast_parent;
		Scheme_Object* ast_child;
		Scheme_Object* ast_sibling;
		Scheme_Object* ast_children;
		Scheme_Object* ast_for_each_child;
		Scheme_Object* ast_find_child;
		Scheme_Object* ast_find_child_m;
		Scheme_Object* ast_has_parent_q;
		Scheme_Object* ast_has_child_q;
		Scheme_Object* ast_has_sibling_q;
		Scheme_Object* ast_child_index;
		Scheme_Object* ast_num_children;
		Scheme_Object* ast_node_type;
		Scheme_Object* ast_list_node_q;
		Scheme_Object* ast_bud_node_q;
		Scheme_Object* ast_subtype_q;
		Scheme_Object* specify_attribute;
		Scheme_Object* compile_ag_specifications;
		Scheme_Object* att_value;
		Scheme_Object* rewrite_terminal;
		Scheme_Object* rewrite_refine;
		Scheme_Object* rewrite_abstract;
		Scheme_Object* rewrite_subtree;
		Scheme_Object* rewrite_add;
		Scheme_Object* rewrite_insert;
		Scheme_Object* rewrite_delete;
		Scheme_Object* perform_rewrites;
		Scheme_Object* ast_annotation_set_e;
		Scheme_Object* ast_weave_annotations;
		Scheme_Object* ast_annotation_remove_e;
		Scheme_Object* ast_annotation_q;
		Scheme_Object* ast_annotation;
	};
} racr;


static void load_bytecode(Scheme_Env* env, const char* bc_file) {
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


static Scheme_Env* global_env;


Scheme_Env* racr_init(void* stack_addr, const char* bytecode, char const** module_names) {
	scheme_set_stack_base(stack_addr, 1);
	Scheme_Env* env = global_env = scheme_basic_env();
	load_bytecode(env, bytecode);

	scheme_namespace_require(scheme_intern_symbol("racket/base"));
	scheme_namespace_require(scheme_intern_symbol("rnrs"));
	scheme_namespace_require(scheme_intern_symbol("racr"));
	if (module_names) {
		for (; *module_names; module_names++) {
			scheme_namespace_require(scheme_intern_symbol(*module_names));
		}
	}

	int i;
	for (i = 0; i < sizeof(racr_function_strings) / sizeof(*racr_function_strings); i++) {
		racr.array[i] = scheme_eval_string(racr_function_strings[i], env);
	}

	return env;
}





static int got_exception = 0;
static void(*exception_handler)(void) = NULL;


int racr_got_exception(void) {
	return got_exception;
}


void racr_set_exception_handler(void(*f)(void)) {
	exception_handler = f;
}


Scheme_Object* racr_build_bounds(int len, int bounds[][2]) {
	Scheme_Object* b[len];
	int i;
	for (i = 0; i < len; i++) {
		Scheme_Object* args[] = {
			scheme_make_integer(bounds[i][0]),
			(bounds[i][1] > 0)
				? scheme_make_integer(bounds[i][1])
				: scheme_intern_symbol("*")
		};
		b[i] = scheme_apply(racr.cons, 2, args);
	}
	return scheme_build_list(len, b);
}


static Scheme_Object* vracr_call(Scheme_Object* func, const char* fmt, va_list ap) {

	Scheme_Object* o;
	Scheme_Object* rest = NULL;
	mz_jmp_buf * volatile save, fresh;
	save = scheme_current_thread->error_buf;
	scheme_current_thread->error_buf = &fresh;

	if (!scheme_setjmp(scheme_error_buf)) {

		Scheme_Object* args[16];
		int count;
		for (count = 0; fmt[count] != '\0'; count++) {

			if (count >= 16) error(1, 0, "too many arguments");
			switch (fmt[count]) {

			case 'i':
				o = scheme_make_integer(va_arg(ap, int));
				break;

			case 'd':
				o = scheme_make_double(va_arg(ap, double));
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

		if (!rest) o = scheme_apply(func, count, args);
		else {
			args[0] = scheme_apply(racr.list, count - 1, args);
			args[1] = rest;
			args[1] = scheme_apply(racr.append, 2, args);
			args[0] = func;
			o = scheme_apply(racr.apply, 2, args);
		}
		got_exception = 0;
	}
	else {
		o = NULL;
		got_exception = 1;
		if (exception_handler) exception_handler();
	}
	scheme_current_thread->error_buf = save;
	return o;
}


Scheme_Object* racr_call(Scheme_Object* func, const char* fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	Scheme_Object* o = vracr_call(func, fmt, ap);
	va_end(ap);
	return o;
}


Scheme_Object*	racr_call_str(const char* func, const char* fmt, ...) {
	Scheme_Object* f = scheme_eval_string(func, global_env);
	va_list ap;
	va_start(ap, fmt);
	Scheme_Object* o = vracr_call(f, fmt, ap);
	va_end(ap);
	return o;
}


Scheme_Object*	racr_list(const char* fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	Scheme_Object* o = vracr_call(racr.list, fmt, ap);
	va_end(ap);
	return o;
}


Scheme_Object* racr_create_specification(void) {
	return racr_call(racr.create_specification, "");
}

// Abstract Syntax Trees
void racr_ast_rule(Scheme_Object* spec, const char* symbol_encoding_rule) {
	racr_call(racr.ast_rule, "*s", spec, symbol_encoding_rule);
}
void racr_compile_ast_specifications(Scheme_Object* spec, const char* start_symbol) {
	racr_call(racr.compile_ast_specifications, "*s", spec, start_symbol);
}
int racr_is_ast_node(Scheme_Object* scheme_entity) {
	return SCHEME_TRUEP(racr_call(racr.ast_node_q, "*", scheme_entity));
}
Scheme_Object* racr_create_ast(
		Scheme_Object* spec,
		const char* non_terminal,
		Scheme_Object* list_of_children) {
	return racr_call(racr.create_ast, "*s*",
		spec,
		non_terminal,
		list_of_children);
}
Scheme_Object* racr_create_ast_list(Scheme_Object* list_of_children) {
	return racr_call(racr.create_ast_list, "*", list_of_children);
}
Scheme_Object* racr_create_ast_bud(void) {
	return racr_call(racr.create_ast_bud, "");
}
Scheme_Object* racr_ast_parent(Scheme_Object* n) {
	return racr_call(racr.ast_parent, "*", n);
}
Scheme_Object* racr_ast_child_by_index(int index, Scheme_Object* n) {
	return racr_call(racr.ast_child, "i*", index, n);
}
Scheme_Object* racr_ast_child_by_name(const char* name, Scheme_Object* n) {
	return racr_call(racr.ast_child, "s*", name, n);
}
Scheme_Object* racr_ast_sibling_by_index(int index, Scheme_Object* n) {
	return racr_call(racr.ast_sibling, "i*", index, n);
}
Scheme_Object* racr_ast_sibling_by_name(const char* name, Scheme_Object* n) {
	return racr_call(racr.ast_sibling, "*s", name, n);
}
Scheme_Object* racr_ast_children(Scheme_Object* n, Scheme_Object* b) {
	return racr_call(racr.ast_children, "*.", n, b);
}
void racr_ast_for_each_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	racr_call(racr.ast_for_each_child, "**.", f, n, b);
}
Scheme_Object* racr_ast_find_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	return racr_call(racr.ast_find_child, "**.", f, n, b);
}
Scheme_Object* racr_ast_find_child_(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b) {
	return racr_call(racr.ast_find_child_m, "**.", f, n, b);
}
int racr_ast_has_parent(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call(racr.ast_has_parent_q, "*", n));
}
int racr_ast_has_child(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call(racr.ast_has_child_q, "*", n));
}
int racr_ast_has_sibling(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call(racr.ast_has_sibling_q, "*", n));
}
int racr_ast_child_index(Scheme_Object* n) {
	return SCHEME_INT_VAL(racr_call(racr.ast_child_index, "*", n));
}
int racr_ast_num_children(Scheme_Object* n) {
	return SCHEME_INT_VAL(racr_call(racr.ast_num_children, "*", n));
}
const char* racr_ast_node_type(Scheme_Object* n) {
	return SCHEME_SYM_VAL(racr_call(racr.ast_node_type, "*", n));
}
int racr_is_ast_list_node(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call(racr.ast_list_node_q, "*", n));
}
int racr_is_ast_bud_node(Scheme_Object* n) {
	return SCHEME_TRUEP(racr_call(racr.ast_bud_node_q, "*", n));
}
int racr_is_ast_subtype_nn(Scheme_Object* a1, Scheme_Object* a2) {
	return SCHEME_TRUEP(racr_call(racr.ast_subtype_q, "**", a1, a2));
}
int racr_is_ast_subtype_nt(Scheme_Object* a, const char* t) {
	return SCHEME_TRUEP(racr_call(racr.ast_subtype_q, "*s", a, t));
}
int racr_is_ast_subtype_tn(const char* t, Scheme_Object* a) {
	return SCHEME_TRUEP(racr_call(racr.ast_subtype_q, "s*", t, a));
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
	racr_call(racr.specify_attribute, "*ssib**",
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
	racr_call(racr.specify_attribute, "*ss*b**",
		spec,
		att_name,
		non_terminal,
		name,
		is_cached,
		equation,
		circ_def);
}
void racr_compile_ag_specifications(Scheme_Object* spec) {
	racr_call(racr.compile_ag_specifications, "*", spec);
}
Scheme_Object* racr_att_value(
		const char* attribute_name,
		Scheme_Object* node,
		Scheme_Object* arguments) {
	return racr_call(racr.att_value, "s*.",
		attribute_name,
		node,
		arguments);
}

// Rewriting
Scheme_Object* racr_rewrite_terminal(int i, Scheme_Object* n, Scheme_Object* new_value) {
	return racr_call(racr.rewrite_terminal, "i**", i, n, new_value);
}
void racr_rewrite_refine(Scheme_Object* n, const char* t, Scheme_Object* c) {
	racr_call(racr.rewrite_refine, "*s.", n, t, c);
}
Scheme_Object* racr_rewrite_abstract(Scheme_Object* n, const char* t) {
	return racr_call(racr.rewrite_abstract, "*s", n, t);
}
Scheme_Object* racr_rewrite_subtree(
		Scheme_Object* old_fragment,
		Scheme_Object* new_fragment) {
	return racr_call(racr.rewrite_subtree, "**", old_fragment, new_fragment);
}
void racr_rewrite_add(Scheme_Object* l, Scheme_Object* e) {
	racr_call(racr.rewrite_add, "**", l, e);
}
void racr_rewrite_insert(Scheme_Object* l, int i, Scheme_Object* e) {
	racr_call(racr.rewrite_insert, "*i*", l, i, e);
}
Scheme_Object* racr_rewrite_delete(Scheme_Object* n) {
	return racr_call(racr.rewrite_delete, "*", n);
}
Scheme_Object* racr_perform_rewrites(
		Scheme_Object* n,
		Scheme_Object* strategy,
		Scheme_Object* transformers) {
	return racr_call(racr.perform_rewrites, "**.", n, strategy, transformers);
}

// AST Annotations
void racr_ast_annotation_set(Scheme_Object* n, const char* a, Scheme_Object* v) {
	racr_call(racr.ast_annotation_set_e, "*s*", n, a, v);
}
void racr_ast_weave_annotations(Scheme_Object* n, const char* t, const char* a, Scheme_Object* v) {
	racr_call(racr.ast_weave_annotations, "*ss*", n, t, a, v);
}
void racr_ast_annotation_remove(Scheme_Object* n, const char* a) {
	racr_call(racr.ast_annotation_remove_e, "*s", n, a);
}
int racr_is_ast_annotation(Scheme_Object* n, const char* a) {
	return SCHEME_TRUEP(racr_call(racr.ast_annotation_q, "*s", n, a));
}
Scheme_Object* racr_ast_annotation(Scheme_Object* n, const char* a) {
	return racr_call(racr.ast_annotation, "*s", n, a);
}

