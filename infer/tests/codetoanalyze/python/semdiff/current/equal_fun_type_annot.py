# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

def foo() -> None:
    write_html(
        "file.json"
    )

def write_html(json_file_path:str) -> None: pass
