# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Executes a java program and dumps the names of all allocated classes at runtime.
# We are simply executing hprof and parsing its output.

### Example format of allocation sites table:
# SITES BEGIN (ordered by live bytes) Thu Jul  7 07:08:20 2022
#           percent          live          alloc'ed  stack class
#  rank   self  accum     bytes objs     bytes  objs trace name
#     1  3.39%  3.39%     11464   71     11464    71 300010 char[]
#     2  2.43%  5.83%      8208    1      8208     1 300187 byte[]
# SITES END

import argparse
import os
import subprocess

def get_classnames(hprof_output):
    classes = []
    table_found = False

    for line in hprof_output.splitlines():
        if table_found:
            entries = line.split()
            if len(entries) == 9:
                classes.append(entries[-1])
        elif line == " rank   self  accum     bytes objs     bytes  objs trace name":
            table_found = True

    return sorted(list(set(classes)))

parser = argparse.ArgumentParser(description=
    'Execute jar and produce a list of classes allocated at runtime.')
parser.add_argument('jars', type=str, nargs='+', help='list of jar files')
parser.add_argument('--output', type=str, help='output dir')
args = parser.parse_args()

out_dir = args.output if args.output else "out"
if not os.path.exists(out_dir):
    os.makedirs(out_dir)

for jar in args.jars:
    jar_filename = os.path.basename(jar)
    out_path = out_dir + "/" + jar_filename

    subprocess.run(["java", "-jar", "-agentlib:hprof=heap=sites,file=" + out_path + ".hprof", jar])
    hprof_output = open(out_path + ".hprof", mode="r").read()
    classes = get_classnames(hprof_output)
    f = open(out_path + ".classes", "w")
    for c in classes:
        f.write(c + "\n")
