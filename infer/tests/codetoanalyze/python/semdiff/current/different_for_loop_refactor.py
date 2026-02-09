# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Example with for loop refactoring
def main():
    temp_dict = dict(o.mapping)
    for i in range(7, 13):
        temp_dict[i] -= 7
    o.mapping = temp_dict
    action1()
    action2()
