#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

TSInputEdit edit;

TSTree *ts_edit_tree_and_parse(
    TSParser *parser
    , TSTree *tree
    , char *source
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
    return ts_parser_parse_string(parser, tree, source, strlen(source));
}
