#include "tree_sitter/api.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  TSNode node;
  const char *type;
  TSSymbol symbol;
  TSPoint endPoint;
  uint32_t endByte;
  uint32_t childCount;
  const char *fieldName;
  bool     isNamed;
  bool     isExtra;
} Node;

void log_to_stdout(void *payload, TSLogType type, const char *message) {
  printf("%s\n", message);
}

void ts_parser_log_to_stderr(TSParser *parser) {
  ts_parser_set_logger(parser, (TSLogger) {.log = log_to_stdout, .payload = NULL});
}

static inline void ts_node_poke(const char *fieldName, TSNode node, Node *out) {
  out->node = node;
  out->symbol = ts_node_symbol(node);
  out->type = ts_node_type(node);
  out->endPoint = ts_node_end_point(node);
  out->endByte = ts_node_end_byte(node);
  out->childCount = ts_node_child_count(node);
  out->fieldName = fieldName;
  out->isNamed = ts_node_is_named(node);
  out->isExtra = ts_node_is_extra(node);
}

void ts_node_poke_p(TSNode *node, Node *out) {
  assert(node != NULL);
  ts_node_poke(NULL, *node, out);
}

void ts_tree_root_node_p(TSTree *tree, Node *outNode) {
  assert(tree != NULL);
  assert(outNode != NULL);
  TSNode root = ts_tree_root_node(tree);
  assert(root.id != NULL);
  ts_node_poke(NULL, root, outNode);
}

void ts_node_copy_child_nodes(const TSNode *parentNode, Node *outChildNodes) {
  assert(parentNode != NULL);
  assert(outChildNodes != NULL);
  TSTreeCursor curse = ts_tree_cursor_new(*parentNode);

  if (ts_tree_cursor_goto_first_child(&curse)) {
    do {
      TSNode current = ts_tree_cursor_current_node(&curse);
      ts_node_poke(ts_tree_cursor_current_field_name(&curse), current, outChildNodes);
      outChildNodes++;
    } while (ts_tree_cursor_goto_next_sibling(&curse));
  }

  ts_tree_cursor_delete(&curse);
}

size_t sizeof_tsnode() {
  return sizeof(TSNode);
}

size_t sizeof_tspoint() {
  return sizeof(TSPoint);
}

size_t sizeof_node() {
  return sizeof(Node);
}

size_t sizeof_tstreecursor() {
  return sizeof(TSTreeCursor);
}


void ts_tree_cursor_new_p(TSNode *node, TSTreeCursor *outCursor) {
  assert(node != NULL);
  assert(outCursor != NULL);
  *outCursor = ts_tree_cursor_new(*node);
}

void ts_tree_cursor_reset_p(TSTreeCursor *cursor, TSNode *node) {
  assert(cursor != NULL);
  assert(node != NULL);
  ts_tree_cursor_reset(cursor, *node);
}

bool ts_tree_cursor_current_node_p(const TSTreeCursor *cursor, Node *outNode) {
  assert(cursor != NULL);
  assert(outNode != NULL);
  TSNode tsNode = ts_tree_cursor_current_node(cursor);
  if (!ts_node_is_null(tsNode)) {
    ts_node_poke(ts_tree_cursor_current_field_name(cursor), tsNode, outNode);
  }
  return false;
}


uint32_t ts_tree_cursor_copy_child_nodes(TSTreeCursor *cursor, Node *outChildNodes) {
  assert(cursor != NULL);
  assert(outChildNodes != NULL);
  uint32_t count = 0;

  if (ts_tree_cursor_goto_first_child(cursor)) {
    do {
      TSNode current = ts_tree_cursor_current_node(cursor);
      const char *fieldName = ts_tree_cursor_current_field_name(cursor);
      if (fieldName || (ts_node_is_named(current) && !ts_node_is_extra(current))) {
        ts_node_poke(fieldName, current, outChildNodes);
        count++;
        outChildNodes++;
      }
    } while (ts_tree_cursor_goto_next_sibling(cursor));
    ts_tree_cursor_goto_parent(cursor);
  }
  return count;
}
