# Charon item selection

## Items and opacity

An **item** is a module, function, type decl, static, const, trait decl, or trait impl. A crate is considered a module.

Each item has an **opacity** level[^1]:

1. **Transparent** — the item is fully translated.
2. **Foreign** — the default for items outside the current crate. Translation depends on normal Rust visibility: for types, translate fully if it's a struct with all-public fields or an enum; for other items this is equivalent to Opaque.
3. **Opaque** — only the name and signature are translated, not the contents. For functions and globals, the body is not translated. For types (structs, enums, unions), the fields/variants are not translated. For traits and trait impls, the default method is  only translated if it is used somewhere else. For modules, the contents are not explored (but items referenced from elsewhere are still translated).
4. **Invisible** — nothing is translated for this item. The corresponding map will not have an entry for the item ID. Useful when even the signature causes errors.

## Selection process

Charon starts from a set of **entry points** and explores outward using a work-queue:

1. The entry point items are enqueued for processing.
2. Each item is processed according to its opacity:
   - **Transparent module**: enqueue all items directly inside it.
   - **Non-transparent module**: do nothing (contents are not explored).
   - **Transparent non-module item**: translate it fully (including body).
   - **Foreign or opaque non-module item**: translate only the signature.
   - **Invisible item**: do nothing.
3. Whenever processing an item encounters a reference to a new item, that new item is enqueued for processing.

This is a dependency-driven algorithm that pulls in items as they are needed. With the default settings, the entry point is the current crate (a module), which is transparent, so all its direct items are enqueued. Those items reference foreign items from dependencies, which get enqueued as foreign (signatures only). The result: the whole current crate is translated, plus the signatures of all foreign items it references.

## How opacity is determined

Each item's opacity is calculated from its name by matching against a list of patterns. The patterns come from two sources: CLI flags and source annotations.

### Source annotations

The source-level attributes `#[charon::opaque]` and `#[charon::exclude]` set the opacity of individual items. They **can only make items more opaque, never less**[^2].

When both a source annotation and a CLI pattern apply, the more opaque of the two wins (`Transparent < Foreign < Opaque < Invisible`). This means `--include` **cannot** override a `#[charon::opaque]` annotation.

### CLI flags

- **`--start-from <PATTERN>`** — Entry points for translation. Can be specified multiple times. Default: `crate` (the entire current crate).
- **`--start-from-attribute[=<ATTR>]`** — Use items annotated with a given attribute as entry points. Default attribute if none specified: `verify::start_from`.
- **`--start-from-pub`** — Use all `pub` items from the current crate as entry points
- **`--include <PATTERN>`** — Set matched items to **transparent**.
- **`--opaque <PATTERN>`** — Set matched items to **opaque**.
- **`--exclude <PATTERN>`** — Set matched items to **invisible**.
- **`--extract-opaque-bodies`** — Alias for `--include *`. Makes all items transparent.
- **`--translate-all-methods`** — Include provided trait methods even if unused. Equivalent to making all trait declarations transparent.  

### Pattern list

Charon builds an ordered list of `(pattern, opacity)` pairs from the CLI flags[^3]:

```
_              → Foreign      (default: everything starts as foreign)
crate          → Transparent  (current crate is always transparent)
<--include>    → Transparent  (user whitelist)
<--opaque>     → Opaque       (user blacklist)
<--exclude>    → Invisible    (user exclusion)
```

The default is equivalent to `--opaque '*' --include crate`: everything is foreign except items in the current crate which are transparent.

### Pattern syntax

Patterns use a name-matcher syntax (`charon/src/name_matcher/`). Examples:

- `crate::module::item` — matches this item and all its subitems
- `crate::module::item::_` — matches only the subitems (not the item itself)
- `core::convert::{impl core::convert::Into<_> for _}` — a specific trait impl
- `_` or `*` — glob, matches any single path segment
- `{impl core::convert::Into<_> for _}` works without the core::convert prefix, in this case we do a global search for impls.

Patterns are **prefix matches**: `crate::foo` matches `crate::foo`, `crate::foo::bar`, `crate::foo::bar::baz`, etc.[^4]

Currently matching on inherent impl blocks isn't supported, writing `crate::module::_::method` is the standard workaround.

### Precedence

When multiple patterns match the same item, the **most precise pattern wins**[^5]:

1. **Longer patterns** beat shorter ones.
2. Among equal-length patterns, a **non-glob final element** beats a glob.

Example with `--opaque crate::module --include crate::module::_`:

| Item                  | Matching patterns                                  | Most precise                  | Result      |
|-----------------------|----------------------------------------------------|-------------------------------|-------------|
| `crate::module`       | `_`, `crate`, `crate::module`                      | `crate::module` (length 2)    | Opaque      |
| `crate::module::foo`  | `_`, `crate`, `crate::module`, `crate::module::_`  | `crate::module::_` (length 3) | Transparent |

Note: pattern matching currently has limitations — e.g. it parses `u64` as a path instead of the built-in type, and it is not possible to filter a trait impl (only its methods)[^6].

[^1]: `charon/src/ast/meta.rs:191`
[^2]: `charon/src/bin/charon-driver/translate/translate_meta.rs:776-785`
[^3]: `charon/src/options.rs:534-570`
[^4]: `charon/src/name_matcher/mod.rs:51-125`
[^5]: `charon/src/name_matcher/mod.rs:189-221`
[^6]: `charon/src/options.rs:112`
