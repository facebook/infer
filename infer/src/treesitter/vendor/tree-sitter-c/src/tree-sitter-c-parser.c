/* Wrapper to include the grammar parser with a unique name,
   avoiding conflict with the tree-sitter runtime's parser.c
   when both directories are in the -I search path. */
#include "parser.c"
