/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* OCaml C stubs for tree-sitter integration.
 *
 * Parses a C source file using the bundled tree-sitter runtime and C grammar,
 * then walks the CST and builds an OCaml representation (same cst_node type
 * as the XML parser produces).
 *
 * The tree-sitter runtime and C grammar are compiled into the Infer binary
 * via dune foreign_stubs — no external tree-sitter CLI needed.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include "tree_sitter/api.h"

/* Forward declaration of the bundled C grammar */
extern const TSLanguage *tree_sitter_c(void);

/* Read entire file into a malloc'd buffer. Returns NULL on failure. */
static char *read_file(const char *path, uint32_t *out_len) {
  FILE *f = fopen(path, "rb");
  if (!f) return NULL;
  fseek(f, 0, SEEK_END);
  long len = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *buf = malloc(len + 1);
  if (!buf) { fclose(f); return NULL; }
  size_t nread = fread(buf, 1, len, f);
  fclose(f);
  buf[nread] = '\0';
  *out_len = (uint32_t)nread;
  return buf;
}

/*
 * Recursively convert a TSNode into an OCaml cst_node record:
 *
 *   type cst_node = {
 *     tag: string;
 *     field: string option;
 *     srow: int; scol: int; erow: int; ecol: int;
 *     text: string;
 *     children: cst_node list;
 *   }
 */
static value node_to_ocaml(TSNode node, const char *source, const char *field_name) {
  CAMLparam0();
  CAMLlocal5(result, children_list, child_val, cons, field_opt);
  CAMLlocal2(tag_str, text_str);

  const char *type = ts_node_type(node);
  uint32_t srow = ts_node_start_point(node).row;
  uint32_t scol = ts_node_start_point(node).column;
  uint32_t erow = ts_node_end_point(node).row;
  uint32_t ecol = ts_node_end_point(node).column;

  /* Build tag string */
  tag_str = caml_copy_string(type);

  /* Build field option */
  if (field_name) {
    field_opt = caml_alloc(1, 0); /* Some */
    Store_field(field_opt, 0, caml_copy_string(field_name));
  } else {
    field_opt = Val_none;
  }

  /* Build text: for leaf nodes (no named children), extract source text.
   * For internal nodes, collect text between named children (operators etc). */
  /* text_str declared via CAMLlocal2 above */
  uint32_t named_count = ts_node_named_child_count(node);
  if (named_count == 0 && !ts_node_is_null(node)) {
    /* Leaf node: extract source text */
    uint32_t start = ts_node_start_byte(node);
    uint32_t end = ts_node_end_byte(node);
    uint32_t len = end - start;
    char *buf = malloc(len + 1);
    memcpy(buf, source + start, len);
    buf[len] = '\0';
    text_str = caml_copy_string(buf);
    free(buf);
  } else {
    /* Internal node: collect non-child text (operators, keywords, punctuation) */
    uint32_t child_count = ts_node_child_count(node);
    uint32_t node_start = ts_node_start_byte(node);
    uint32_t node_end = ts_node_end_byte(node);

    /* Collect text segments between children */
    char *text_buf = malloc(node_end - node_start + 1);
    uint32_t text_len = 0;
    uint32_t pos = node_start;

    for (uint32_t i = 0; i < child_count; i++) {
      TSNode child = ts_node_child(node, i);
      uint32_t child_start = ts_node_start_byte(child);

      /* Text before this child */
      if (child_start > pos) {
        uint32_t gap = child_start - pos;
        memcpy(text_buf + text_len, source + pos, gap);
        text_len += gap;
      }
      pos = ts_node_end_byte(child);
    }
    /* Text after last child */
    if (node_end > pos) {
      uint32_t gap = node_end - pos;
      memcpy(text_buf + text_len, source + pos, gap);
      text_len += gap;
    }

    /* Strip whitespace */
    while (text_len > 0 && (text_buf[text_len-1] == ' ' || text_buf[text_len-1] == '\n'
           || text_buf[text_len-1] == '\t' || text_buf[text_len-1] == '\r'))
      text_len--;
    uint32_t start_trim = 0;
    while (start_trim < text_len && (text_buf[start_trim] == ' ' || text_buf[start_trim] == '\n'
           || text_buf[start_trim] == '\t' || text_buf[start_trim] == '\r'))
      start_trim++;

    text_buf[text_len] = '\0';
    text_str = caml_copy_string(text_buf + start_trim);
    free(text_buf);
  }

  /* Build children list (reversed, then reversed back) */
  children_list = Val_emptylist;
  for (int i = (int)named_count - 1; i >= 0; i--) {
    TSNode child = ts_node_named_child(node, i);
    const char *child_field = ts_node_field_name_for_named_child(node, i);
    child_val = node_to_ocaml(child, source, child_field);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, child_val);
    Store_field(cons, 1, children_list);
    children_list = cons;
  }

  /* Allocate the record: {tag; field; srow; scol; erow; ecol; text; children} */
  result = caml_alloc(8, 0);
  Store_field(result, 0, tag_str);
  Store_field(result, 1, field_opt);
  Store_field(result, 2, Val_int(srow));
  Store_field(result, 3, Val_int(scol));
  Store_field(result, 4, Val_int(erow));
  Store_field(result, 5, Val_int(ecol));
  Store_field(result, 6, text_str);
  Store_field(result, 7, children_list);

  CAMLreturn(result);
}

/*
 * OCaml external: parse_file : string -> cst_node
 *
 * Parses the given file path with tree-sitter using the bundled C grammar
 * and returns the root translation_unit node.
 */
CAMLprim value caml_tree_sitter_parse_file(value filename) {
  CAMLparam1(filename);
  CAMLlocal1(result);

  const char *path = String_val(filename);

  uint32_t source_len = 0;
  char *source = read_file(path, &source_len);
  if (!source) {
    caml_failwith("tree_sitter_parse_file: could not read file");
  }

  TSParser *parser = ts_parser_new();
  ts_parser_set_language(parser, tree_sitter_c());

  TSTree *tree = ts_parser_parse_string(parser, NULL, source, source_len);
  if (!tree) {
    free(source);
    ts_parser_delete(parser);
    caml_failwith("tree_sitter_parse_file: parse failed");
  }

  TSNode root = ts_tree_root_node(tree);
  result = node_to_ocaml(root, source, NULL);

  ts_tree_delete(tree);
  ts_parser_delete(parser);
  free(source);

  CAMLreturn(result);
}
