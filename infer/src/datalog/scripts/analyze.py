# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Generates facts using Infer and feeds them to the Souffle
# For this to work, you need to have Infer in your $PATH and Souffle installed

import argparse
import os
import sys
import subprocess
from shutil import which

parser = argparse.ArgumentParser(description=
    """
    Generates facts using Infer and feeds them to the Souffle.
    The results are found in <output_dir>/analysis_results.
    """)
parser.add_argument('sourcefiles', type=str, nargs='+', help='list of java source files to analyse')
parser.add_argument('--output', type=str, help='output dir')
parser.add_argument('--infer-args', type=str,
    help='extra arguments for Infer separated with commas')
parser.add_argument('--souffle-args', type=str,
    help='extra arguments for Souffle separated with commas')
args = parser.parse_args()
if which("souffle") is None:
    sys.exit("Please install Souffle and make sure it is included in $PATH")
if which("infer") is None:
    sys.exit("Please install Infer and make sure it is included in $PATH")

out_dir = args.output if args.output else "infer-out"
fact_dir = out_dir + "/facts"
analysis_output_dir = out_dir + "/analysis_results"
inference_rules_path = os.path.realpath(os.path.dirname(__file__)) + "/../Analysis.dl"

infer_cmd = ["infer", "run", "--datalog-only", "-o", out_dir]
if args.infer_args:
    infer_cmd.extend([arg for arg in args.infer_args.split(',')])
else:
    infer_cmd.extend(["--quiet"])
infer_cmd.extend(["--", "javac"])
infer_cmd.extend(args.sourcefiles)

souffle_cmd = ["souffle", "-F", fact_dir, "-D", analysis_output_dir]
if args.souffle_args:
    souffle_cmd.extend([arg for arg in args.souffle_args.split(',')])
souffle_cmd.extend([inference_rules_path])

subprocess.run(infer_cmd, check=True)
print("Infer: completed")
if not os.path.exists(analysis_output_dir):
    os.mkdir(analysis_output_dir)
subprocess.run(souffle_cmd, check=True)
print("Souffle: completed")
