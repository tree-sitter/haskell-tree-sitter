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
    updateNode(p, n);
}

void updateNode(Cursor *p, TSNode n)
{
    p->type = ts_node_type(n);
    p->symbol = ts_node_symbol(n);
    p->startPoint = ts_node_start_point(n);
    p->endPoint = ts_node_end_point(n);
}

void ts_edit_node_in_range(TSTree *tree, uint32_t rangeFrom, uint32_t rangeTo, TSInputEdit *edit)
{
    assert(tree != NULL);
    TSNode rootNode = ts_tree_root_node(tree);
    assert(rootNode.id != NULL);
    // TSNode node = ts_node_descendant_for_byte_range(rootNode, rangeFrom, rangeTo);
    // assert(node.id != NULL);
    // printf("node in range: %s \n", ts_node_type(node));
    // ts_node_edit(&node, edit);
    ts_node_edit(&rootNode, edit);
}

void ts_cursor_init(TSTree *tree, Cursor *p)
{
    assert(tree != NULL);
    TSNode rootNode = ts_tree_root_node(tree);
    assert(rootNode.id != NULL);
    icur.node = rootNode;
    icur.cursor = ts_tree_cursor_new(rootNode);
    updateNode(p, rootNode);
}

void ts_cursor_reset_root(TSTree *tree, Cursor *p)
{
    assert(tree != NULL);
    TSNode rootNode = ts_tree_root_node(tree);
    assert(rootNode.id != NULL);
    icur.node = rootNode;
    ts_tree_cursor_reset(&icur.cursor, rootNode);
    updateNode(p, rootNode);
}

bool ts_cursor_goto_first_child(Cursor *p)
{
    if (ts_tree_cursor_goto_first_child(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

bool ts_cursor_goto_next_sibling(Cursor *p)
{
    if (ts_tree_cursor_goto_next_sibling(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

bool ts_cursor_goto_parent(Cursor *p)
{
    if (ts_tree_cursor_goto_parent(&icur.cursor))
    {
        updateCursor(p);
        return true;
    }
    return false;
}

bool ts_cursor_has_children()
{
    return ts_node_child_count(icur.node) > 0;
}

void ts_cursor_free(Cursor *p)
{
    ts_tree_cursor_delete(&icur.cursor);
}

