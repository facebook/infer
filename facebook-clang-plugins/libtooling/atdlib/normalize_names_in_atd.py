#!/usr/bin/env python3


# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import sys
import re
import argparse

"""
This script normalizes the names between "@" across the file + removes empty lines.
"""

bigcap = re.compile("([A-Z]*)([A-Z])")

def normalize_name(name, first_letter_in_bigcap = False):
    """
    Convert a name in java-like convention to the small-caps + underscores convention.
    Examples of renaming:
      ThisName -> this_name
      CXXDecl -> cxx_decl
    """
    name = name.strip()
    if name == "":
        return

    def f_sub(m):
        res = "";
        if m.start() != 0:
            res += "_"
        if m.group(1) != "":
            res += m.group(1).lower()
            res += "_"
        res += m.group(2).lower()
        return res

    name = bigcap.sub(f_sub, name)

    if first_letter_in_bigcap:
        name = name[0].upper() + name[1:]
    return name

at_word = re.compile("@(.*?)@")

def start(file):
    for line in file:
        if not line.strip():
            continue
        line = at_word.sub(lambda m: normalize_name(m.group(1)), line)
        sys.stdout.write(line)

def main():
    arg_parser = argparse.ArgumentParser(description='Normalize the strings "@AxxByy@" to "axx_byy" across the ATD file and remove empty lines.')
    arg_parser.add_argument(metavar="FILE", nargs='?', dest="input_file", help="Input log file (default: stdin)")
    args = arg_parser.parse_args()
    if args.input_file:
        file = open(args.input_file, "r")
    else:
        file = sys.stdin
    start(file)

if __name__ == '__main__':
    main()
