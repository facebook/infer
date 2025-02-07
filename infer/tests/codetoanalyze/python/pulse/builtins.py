# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


from pathlib import Path


def main():
    print(int("0b110", 2))
    print(int("42", 10))
    print(str("42"))
    print(str(Path("/my/dir/", "test.py"), "utf-8"))
