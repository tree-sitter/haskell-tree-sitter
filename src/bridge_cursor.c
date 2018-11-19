#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "ts_cursor.h"

InternalCursor icur;

void debugPrintCurrentNode(Cursor *p)
{
    printf("type: %s \n", p->type);
}

void updateCursor(Cursor *p)
{
    TSNode n = ts_tree_cursor_current_node(&icur.cursor);
    icur.node = n;
    p->type = ts_node_type(n);
}

void ts_ptr_init(TSTree *tree, Cursor *p)
{
    assert(tree != NULL);
    TSNode rootNode = ts_tree_root_node(tree);
    assert(rootNode.id != NULL);
    icur.node = rootNode;
    icur.cursor = ts_tree_cursor_new(rootNode);
    p->type = ts_node_type(rootNode);
}

bool ts_ptr_goto_first_child(Cursor *p)
{
    if (ts_tree_cursor_goto_first_child(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

bool ts_ptr_goto_next_sibling(Cursor *p)
{
    if (ts_tree_cursor_goto_next_sibling(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

bool ts_ptr_goto_parent(Cursor *p)
{
    if (ts_tree_cursor_goto_parent(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

void ts_ptr_free(Cursor *p)
{
    ts_tree_cursor_delete(&icur.cursor);
}

