#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "tree_sitter_ptr.h"

void debugPrintCurrentNode(tree_sitter_ptr *p)
{
    TSNode n = ts_tree_cursor_current_node(&p->cursor);
    printf("type: %s \n", ts_node_type(n));
}

void ts_ptr_init(TSTree *tree, tree_sitter_ptr *p)
{
    assert(tree != NULL);
    TSNode rootNode = ts_tree_root_node(tree);
    assert(rootNode.id != NULL);
    p->rootNode = rootNode;
    p->cursor = ts_tree_cursor_new(p->rootNode);
}

bool ts_ptr_goto_first_child(tree_sitter_ptr *p)
{
    if (ts_tree_cursor_goto_first_child(&p->cursor))
    {
        // debugPrintCurrentNode(p);
        return true;
    }
    return false;
}

bool ts_ptr_goto_next_sibling(tree_sitter_ptr *p)
{
    if (ts_tree_cursor_goto_next_sibling(&p->cursor))
    {
        // debugPrintCurrentNode(p);
        return true;
    }
    return false;
}

bool ts_ptr_goto_parent(tree_sitter_ptr *p)
{
    if (ts_tree_cursor_goto_parent(&p->cursor))
    {
        // debugPrintCurrentNode(p);
        return true;
    }
    return false;
}

const char *ts_ptr_current_type(tree_sitter_ptr *p)
{
    TSNode n = ts_tree_cursor_current_node(&p->cursor);
    // debugPrintCurrentNode(p);
    return ts_node_type(n);
}

void ts_ptr_free(tree_sitter_ptr *p)
{
    ts_tree_cursor_delete(&p->cursor);
}
