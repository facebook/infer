# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
import re
import subprocess
import sys

line_marker = ">>>>>LINE"
output_marker = "#stdout: "


def insert_marker_before_each_print(input, output):
    with open(input, "r") as f:
        lines = f.readlines()

    with open(output, "w") as f:
        count = 0
        for line in lines:
            if line.startswith("print("):
                f.write(f'print("{line_marker} {count}")\n')
            f.write(line)
            count += 1


def insert_local_output(filename, local_ouput_dict):
    with open(filename, "r") as f:
        lines = f.readlines()

    with open(filename, "w") as f:
        count = 0
        for line in lines:
            if line.startswith("print(") and count in local_ouput_dict:
                f.write(line)
                output = local_output_dict[count]
                for output_line in output:
                    f.write(f"{output_marker}{output_line}\n")
            elif not line.startswith(output_marker):
                f.write(line)
            count += 1


def run_python_file(filename):
    process = subprocess.Popen(["python3", filename], stdout=subprocess.PIPE)
    output, _ = process.communicate()
    return output.decode("utf-8").split("\n")


def parse_output(lines):
    buffer = []
    current = None
    dict = {}
    for line in lines:
        match = re.match(line_marker + r" (\d+)", line)
        if match:
            line = int(match.group(1))
            if current:
                dict[current] = buffer
            buffer = []
            current = line
        else:
            buffer.append(line)
    if current:
        if buffer and buffer[-1] == "":
            # remove the superfluous empty line
            buffer = buffer[0:-1]
        dict[current] = buffer
    return dict


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("One argument needed")
        sys.exit(1)

    filename = sys.argv[1]
    filename_with_marker = filename + ".temp"
    insert_marker_before_each_print(input=filename, output=filename_with_marker)
    stdout = run_python_file(filename_with_marker)
    local_output_dict = parse_output(stdout)
    insert_local_output(filename, local_output_dict)
