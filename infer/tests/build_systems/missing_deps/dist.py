#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import subprocess
import sys


INFER=os.environ.get('INFER_BIN')
ROOT=sys.path[0]
SOURCES=['src/a.c','src/b.c']
SOURCE_CAPTURE=ROOT + '/source-out'
MERGE_ARGS_FILE=ROOT + '/merge-args'

def get_cmd_output(cmd):
    return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout

def run(cmd):
    subprocess.run(cmd, check=True)

def capture():
    run([INFER, 'capture', '-o', SOURCE_CAPTURE, '--buck', '--', 'clang', '-c'] + SOURCES)

def analyze(results_dir, index_file):
    run([INFER, 'analyze', '-o', results_dir, '--log-missing-deps', '--no-progress-bar',
        '--no-report', '--changed-files-index', index_file])

def extract(results_dir, index_file, source):
    run([INFER, 'debug', '--source-files', '-o', results_dir,
        '--changed-files-index', index_file, '--extract-capture-from', source])

def complete(results_dir, index_file, source):
    cmd = [INFER, 'debug', '--source-files', '-o', results_dir,
        '--changed-files-index', index_file, '--complete-capture-from', source, '--debug-level=2']
    return get_cmd_output(cmd).strip() == "UNMODIFIED"


def main():
    extra_args = []

    capture()

    for idx, file in enumerate(SOURCES):
        results_dir = ROOT + '/' + str(idx) + '-out'

        index_file = ROOT + '/' + str(idx) + '.index'
        with open(index_file, 'w') as f:
            f.write(file)

        extract(results_dir, index_file, SOURCE_CAPTURE)

        iterations = 0
        while True:
            iterations += 1

            analyze(results_dir, index_file)
            if complete(results_dir, index_file, SOURCE_CAPTURE):
                break

        extra_args.extend(['--merge-summaries', results_dir])

    subprocess.run([INFER, 'report'] + extra_args)

main()
