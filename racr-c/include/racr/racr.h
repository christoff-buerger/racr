#include <scheme.h>
#include <stdarg.h>


Scheme_Env* racr_init(void* stack_addr);
#define RACR_INIT(env)\
	int _dummy;\
	Scheme_Env* __attribute__((unused)) env = racr_init(&_dummy);


// racr helper functions
Scheme_Object* racr_call(const char* mod, const char* func, const char* fmt, ...);
Scheme_Object* racr_build_bounds(int len, int bounds[][2]);

Scheme_Object* racr_create_specification(void);

// Abstract Syntax Trees
void racr_ast_rule(Scheme_Object* spec, const char* symbol_encoding_rule);
void racr_compile_ast_specifications(Scheme_Object* spec, const char* start_symbol);
int racr_is_ast_node(Scheme_Object* scheme_entity);
Scheme_Object* racr_create_ast(Scheme_Object* spec, const char* non_terminal, Scheme_Object* list_of_children);
Scheme_Object* racr_create_ast_list(Scheme_Object* list_of_children);
Scheme_Object* racr_create_ast_bud(void);
Scheme_Object* racr_ast_parent(Scheme_Object* n);
Scheme_Object* racr_ast_child_by_index(Scheme_Object* n, int index);
Scheme_Object* racr_ast_child_by_name(Scheme_Object* n, const char* name);
Scheme_Object* racr_ast_sibling_by_index(Scheme_Object* n, int index);
Scheme_Object* racr_ast_sibling_by_name(Scheme_Object* n, const char* name);
// TODO: define-syntax
// Scheme_Object* racr_ast_children(Scheme_Object* n, Scheme_Object* b);
// void racr_ast_for_each_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b);
// Scheme_Object* racr_ast_find_child(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b);
// Scheme_Object* racr_ast_find_child_(Scheme_Object* f, Scheme_Object* n, Scheme_Object* b);
// TODO: missing functions
// int racr_ast_has_parent(Scheme_Object* n);
// int racr_ast_has_child(Scheme_Object* n);
// int racr_ast_has_sibling(Scheme_Object* n);
int racr_ast_child_index(Scheme_Object* n);
int racr_ast_num_children(Scheme_Object* n);
const char* racr_ast_node_type(Scheme_Object* n);
int racr_is_ast_list_node(Scheme_Object* n);
int racr_is_ast_bud_node(Scheme_Object* n);
int racr_is_ast_subtype_nn(Scheme_Object* a1, Scheme_Object* a2);
int racr_is_ast_subtype_nt(Scheme_Object* a, const char* t);
int racr_is_ast_subtype_tn(const char* t, Scheme_Object* a);

// Attribution
void racr_specify_attribute_by_index(Scheme_Object* spec, const char* att_name,
		const char* non_terminal, int index, int is_cached,
		Scheme_Object* equation, Scheme_Object* circ_def);
void racr_specify_attribute_by_name(Scheme_Object* spec, const char* att_name,
		const char* non_terminal, const char* name, int is_cached,
		Scheme_Object* equation, Scheme_Object* circ_def);
void racr_compile_ag_specifications(Scheme_Object* spec);
Scheme_Object* racr_att_value(Scheme_Object* spec, const char* attribute_name,
		Scheme_Object* node, Scheme_Object* arguments);

// Rewriting
Scheme_Object* racr_rewrite_terminal(int i, Scheme_Object* n, Scheme_Object* new_value);
void racr_rewrite_refine(Scheme_Object* n, const char* t, Scheme_Object* c);
Scheme_Object* racr_rewrite_abstract(Scheme_Object* n, const char* t);
Scheme_Object* racr_rewrite_subtree(Scheme_Object* old_fragment, Scheme_Object* new_fragment);
void racr_rewrite_add(Scheme_Object* l, Scheme_Object* e);
void racr_rewrite_insert(Scheme_Object* l, int i, Scheme_Object* e);
Scheme_Object* racr_rewrite_delete(Scheme_Object* n);
Scheme_Object* racr_perform_rewrites(Scheme_Object* n, Scheme_Object* strategy, Scheme_Object* transformers);

// AST Annotations
void racr_ast_annotation_set(Scheme_Object* n, const char* a, Scheme_Object* v);
void racr_ast_weave_annotations(Scheme_Object* n, const char* t, const char* a, Scheme_Object* v);
void racr_ast_annotation_remove(Scheme_Object* n, const char* a);
int racr_is_ast_annotation(Scheme_Object* n, const char* a);
Scheme_Object* racr_ast_annotation(Scheme_Object* n, const char* a);


