#include "tree_sitter/runtime.h"

typedef struct InternalCursor {
    TSTreeCursor cursor;
    TSNode node;
} InternalCursor;

typedef struct Cursor {
    const char *type;
    TSSymbol symbol;
    TSPoint startPoint;
    TSPoint endPoint;
    uint32_t startByte;
    uint32_t endByte;
    const void *id;
} Cursor;
