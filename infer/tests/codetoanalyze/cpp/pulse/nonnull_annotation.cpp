/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace nonnull_annotation {

using nonnull_string = const char* _Nonnull;

// functions without a body: their declarations are all we know about them
struct Parser {
  Parser(const char* _Nonnull s);
  int parse(const char* _Nonnull s);
  int parse_nullable(const char* _Nullable s);
  // nonnull() counts the implicit object parameter as 1, so 3 designates [s]
  int parse_attribute(int n, const char* s) __attribute__((nonnull(3)));
  [[gnu::nonnull(2)]] int parse_gnu_attribute(const char* s, const char* t);
  [[gnu::nonnull]] int parse_gnu_attribute_all(const char* s);
  int parse_alias(nonnull_string s);
  static int parse_static(const char* s) __attribute__((nonnull(1)));
};

int free_function(const char* _Nonnull s);

template <typename T>
int template_function(const T* _Nonnull p);

int null_to_free_function_bad() {
  const char* s = nullptr;
  return free_function(s);
}

int null_to_method_bad(Parser& p) {
  const char* s = nullptr;
  return p.parse(s);
}

int null_to_nullable_method_param_ok(Parser& p) {
  const char* s = nullptr;
  return p.parse_nullable(s);
}

int null_to_method_nonnull_attribute_bad(Parser& p) {
  const char* s = nullptr;
  return p.parse_attribute(0, s);
}

int null_to_method_gnu_nonnull_attribute_bad(Parser& p) {
  const char* s = nullptr;
  return p.parse_gnu_attribute(s, "x");
}

int null_to_param_not_in_method_gnu_nonnull_attribute_ok(Parser& p) {
  const char* s = nullptr;
  return p.parse_gnu_attribute("x", s);
}

int null_to_method_gnu_nonnull_attribute_all_bad(Parser& p) {
  const char* s = nullptr;
  return p.parse_gnu_attribute_all(s);
}

int null_to_method_alias_param_bad(Parser& p) {
  const char* s = nullptr;
  return p.parse_alias(s);
}

int null_to_static_method_nonnull_attribute_bad() {
  const char* s = nullptr;
  return Parser::parse_static(s);
}

void null_to_constructor_bad() {
  const char* s = nullptr;
  Parser p(s);
}

int null_to_template_function_bad() {
  const int* p = nullptr;
  return template_function(p);
}

int non_null_to_method_ok(Parser& p) { return p.parse("x"); }

} // namespace nonnull_annotation
