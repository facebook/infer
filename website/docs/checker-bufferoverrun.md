---
title: "Buffer Overrun Analysis (InferBO)"
description: "InferBO is a detector for out-of-bounds array accesses."
---

InferBO is a detector for out-of-bounds array accesses.

Activate with `--bufferoverrun`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes

You can read about its origins in this [blog post](https://research.fb.com/inferbo-infer-based-buffer-overrun-analyzer/).

## List of Issue Types

The following issue types are reported by this checker:
