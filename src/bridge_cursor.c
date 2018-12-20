#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "ts_cursor.h"

InternalCursor icur;
TSParser *parser;

TSTree *hts_parser_parse_string(const char *source, uint32_t len) {
    return ts_parser_parse_string_encoding(parser, NULL, source, len, TSInputEncodingUTF8);
}

TSTree *hts_parse_with_language(const TSLanguage *language, const char *source, uint32_t len) {
    parser = ts_parser_new();
    bool set = ts_parser_set_language(parser, language);
    assert(set == true);
    return ts_parser_parse_string_encoding(parser, NULL, source, len, TSInputEncodingUTF8);
}

TSInputEdit edit;

TSTree *ts_edit_tree_and_parse(
    TSTree *tree
    , const char *source
    , uint32_t len
    , uint32_t startByte
    , uint32_t oldEndByte
    , uint32_t newEndByte
    , uint32_t startPointRow
    , uint32_t startPointCol
    , uint32_t oldEndPointRow
    , uint32_t oldEndPointCol
    , uint32_t newEndPointRow
    , uint32_t newEndPointCol
    )
{
    assert(tree != NULL);
    edit.start_byte = startByte;
    edit.old_end_byte = oldEndByte;
    edit.new_end_byte = newEndByte;
    edit.start_point.row = startPointRow;
    edit.start_point.column = startPointCol;
    edit.old_end_point.row = oldEndPointRow;
    edit.old_end_point.column = oldEndPointCol;
    edit.new_end_point.row = newEndPointRow;
    edit.new_end_point.column = newEndPointCol;
    ts_tree_edit(tree, &edit);
    return ts_parser_parse_string_encoding(parser, tree, source, len, TSInputEncodingUTF8);
}

void debugPrintCurrentNode(Cursor *p)
{
    printf("type: %s \n", p->type);
}

void updateNode(Cursor *p, TSNode n)
{
    p->type = ts_node_type(n);
    p->symbol = ts_node_symbol(n);
    p->startByte = ts_node_start_byte(n);
    p->endByte = ts_node_end_byte(n);
    p->startPoint = ts_node_start_point(n);
    p->endPoint = ts_node_end_point(n);
}

void updateCursor(Cursor *p)
{
    TSNode n = ts_tree_cursor_current_node(&icur.cursor);
    icur.node = n;
    updateNode(p, n);
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

bool ts_cursor_has_parent()
{
    TSNode n = ts_node_parent(icur.node);
    return n.id != NULL;
}

bool ts_cursor_has_children()
{
    return ts_node_child_count(icur.node) > 0;
}

void ts_cursor_free(Cursor *p)
{
    ts_tree_cursor_delete(&icur.cursor);
}

