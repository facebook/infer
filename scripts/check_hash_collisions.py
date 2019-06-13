#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import argparse
import json


def load_report(report_filename):
    with open(report_filename, 'r') as file_in:
        return json.load(file_in)


def compute_duplicates(report):
    table = {}
    for e in report:
        bug_hash = e['hash']
        if bug_hash in table:
            table[bug_hash].append(e)
        else:
            table[bug_hash] = [e]
    duplicates = []
    for value in table.values():
        if len(value) > 1:
            duplicates += value
    return duplicates


def save_duplicates(duplicates, output_filename):
    duplicated_types = {}
    for e in duplicates:
        bug_type = e['bug_type']
        if bug_type in duplicated_types:
            duplicated_types[bug_type] += 1
        else:
            duplicated_types[bug_type] = 1
    for bug_type, count in duplicated_types.items():
        print('{} -> {}'.format(bug_type, count))
    with open(output_filename, 'w') as file_out:
        json.dump(duplicates, file_out, indent=2, separators=(',', ': '))


cli_parser = argparse.ArgumentParser()
cli_parser.add_argument('--report', type=str, required=True,
                        help='Infer report')
cli_parser.add_argument('--out', type=str, required=True,
                        help='Output list of duplicates (in JSON)')


if __name__ == '__main__':
    args = cli_parser.parse_args()
    report = load_report(args.report)
    duplicates = compute_duplicates(report)
    save_duplicates(duplicates, args.out)
