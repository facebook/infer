# Hack Semdiff Tests

This directory contains tests for the Hack semantic diff engine. Each test
compares a file pair `previous/<name>.php` vs `current/<name>.php`:

- `equal_*` files should be semantically equivalent (no output)
- `different_*` files should produce a `SEMDIFF MISMATCH` line

## Configuration

The semdiff engine is configured via `hack_type_annotations.py`, passed
to infer with `--semdiff-configuration`. The config file uses a Python
DSL with three kinds of rules:

- `ignore(pattern)` — ignore matching AST nodes during comparison
- `rewrite(lhs=pattern, rhs=pattern)` — normalize AST before comparison
- `accept(lhs=pattern, rhs=pattern, key=[...])` — accept a difference
  when the `lhs` pattern matches the previous AST and the `rhs` pattern
  matches the current AST, restricted to fields listed in `key`

Pattern variables are declared with `var("X")` and can be used in
both `lhs` and `rhs`.

## Finding Hack AST node names

The semdiff engine works on the Hack full-fidelity parse tree produced
by `hh_parse`. To discover the node names and field names to use in
config patterns, run infer in debug mode:

```sh
infer semdiff --debug \
  --semdiff-previous previous/equal_add_param_type.php \
  --semdiff-current current/equal_add_param_type.php \
  -o /tmp/out
```

This prints the AST for both files. For example:

```
AST1: script(
  script_declarations=[
    ...
    function_declaration(
      function_declaration_header=function_declaration_header(
        function_name="greet",
        function_parameter_list=[
          parameter_declaration(
            parameter_name="$name")]))])

AST2: script(
  script_declarations=[
    ...
    function_declaration(
      function_declaration_header=function_declaration_header(
        function_name="greet",
        function_parameter_list=[
          parameter_declaration(
            parameter_name="$name",
            parameter_type=simple_type_specifier(
              simple_type_specifier="string"))]))])
```

Here `parameter_type` is the field that differs. To accept any change
in that field, add an `accept` rule with `key=["parameter_type"]`.

You can also inspect the raw parse tree directly with:

```sh
hh_parse --full-fidelity-json-parse-tree file.php | python3 -m json.tool
```

Node names in the JSON (e.g. `function_declaration`, `parameter_declaration`)
correspond directly to the names used in config patterns. Field names
that are purely syntactic (parentheses, keywords, semicolons) are
automatically filtered out by infer.
