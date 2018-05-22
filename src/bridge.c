#include "tree_sitter/runtime.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  TSNode node;
  const char *type;
  TSSymbol symbol;
  TSPoint startPoint;
  TSPoint endPoint;
  uint32_t startByte;
  uint32_t endByte;
  uint32_t childCount;
} Node;

void log_to_stdout(void *payload, TSLogType type, const char *message) {
  printf("%s\n", message);
}

void ts_parser_log_to_stderr(TSParser *parser) {
  ts_parser_set_logger(parser, (TSLogger) {.log = log_to_stdout, .payload = NULL});
}

static inline Node ts_node_elaborate(TSNode node) {
  return (Node) {
    .node = node,
    .symbol = ts_node_symbol(node),
    .type = ts_node_type(node),
    .startPoint = ts_node_start_point(node),
    .endPoint = ts_node_end_point(node),
    .startByte = ts_node_start_byte(node),
    .endByte = ts_node_end_byte(node),
    .childCount = ts_node_child_count(node)
  };
}

void ts_tree_root_node_p(TSTree *tree, Node *outNode) {
  assert(tree != NULL);
  assert(outNode != NULL);
  TSNode root = ts_tree_root_node(tree);
  assert(root.id != NULL);
  *outNode = ts_node_elaborate(root);
}

void ts_node_copy_child_nodes(const TSNode *parentNode, Node *outChildNodes, size_t count) {
  assert(parentNode != NULL);
  assert(outChildNodes != NULL);
  assert(count >= 0);
  uint32_t maxCount = ts_node_child_count(*parentNode);
  uint32_t max = maxCount <= count ? maxCount : count;
  for (uint32_t i = 0; i < max; i++) {
    outChildNodes[i] = ts_node_elaborate(ts_node_child(*parentNode, i));
  }
}

// For testing cancellation from Haskell.
// Has the same signature as ts_parser_parse_string for convenience.
TSTree *ts_parser_loop_until_cancelled(TSParser *self, const TSTree *old_tree, const char* string, uint32_t length)
{
  while (ts_parser_enabled(self)) {

  }
  return NULL;
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
