#!/usr/bin/env python3

# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


import json
import os
import sys

REPORT_FILENAME = sys.argv[1]


def load_report():
    with open(REPORT_FILENAME, 'r') as file_in:
        return json.load(file_in)


def compute_duplicates(report):
    table = {}
    for e in report:
        bug_hash = e['hash']
        if bug_hash in table:
            table[bug_hash].append(e)
        else:
            table[bug_hash] = [e]
    duplicates = {}
    for key, value in table.items():
        if len(value) >= 1:
            duplicates[key] = value
    return duplicates


def compute_max_hash(duplicates):
    max_length = 0
    max_hash = 0
    for key, value in duplicates.items():
        length = len(value)
        if length > max_length:
            max_length = length
            max_hash = key
    return max_hash


def print_duplicates(duplicates):
    with open('duplicates.txt', 'w') as file_out:
        for key, value in duplicates.items():
            reports = set([(e['bug_type'], e['qualifier']) for e in value])
            messages = ['{} {}'.format(bug, msg) for (bug, msg) in reports]
            if len(messages) > 1:
                file_out.write('\nWith bug hash {}:\n\t{}\n'.format(
                    key,
                    '\n\t'.join(messages)))


report = load_report()
duplicates = compute_duplicates(report)
print_duplicates(duplicates)
max_hash = compute_max_hash(duplicates)
print('max hash: {}'.format(max_hash))
duplicates = [e for e in report if e['hash'] == max_hash]
with open('duplicates.json', 'w') as file_out:
    json.dump(duplicates, file_out, indent=2, separators=(',', ': '))
