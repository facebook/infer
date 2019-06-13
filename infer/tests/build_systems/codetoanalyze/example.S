/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// THIS CODE HAS NO REAL MEANING, DO NOT TRY TO USE IT
        .file "example.S"
        .text
test:
        pushq   %rbp
        movq    %rsp, %rbp
        movl    %edi, -4(%rbp)
        movl    -4(%rbp), %eax
        addl    $1, %eax
        popq    %rbp
        ret
