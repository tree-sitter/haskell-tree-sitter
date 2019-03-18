#include "tree_sitter/api.h"

/* TODO put TSTreeCursor etc. here ! */
typedef struct {
    TSNode rootNode;
    TSTreeCursor cursor;
} tree_sitter_ptr;

void tree_sitter_ptr_init(tree_sitter_ptr *p);
void goto_first_child(TSTree *tree, tree_sitter_ptr *p);
void tree_sitter_ptr_free(tree_sitter_ptr *p);
