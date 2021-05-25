---
title: "Inefficient keySet Iterator"
description: "Check for inefficient uses of iterators that iterate on keys then lookup their values, instead of iterating on key-value pairs directly."
---

Check for inefficient uses of iterators that iterate on keys then lookup their values, instead of iterating on key-value pairs directly.

Activate with `--inefficient-keyset-iterator`.

Supported languages:
- C/C++/ObjC: No
- C#/.Net: Yes
- Erlang: No
- Java: Yes



## List of Issue Types

The following issue types are reported by this checker:
- [INEFFICIENT_KEYSET_ITERATOR](/docs/next/all-issue-types#inefficient_keyset_iterator)
