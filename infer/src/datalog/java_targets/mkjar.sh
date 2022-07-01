#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir -p jar

while read -r file ; do
    name=${file%.*}
    javac "$name.java"
    jar cvfe "$name.jar" "$name" *.class
    rm *.class
    mv "$name.jar" jar
done < <(ls | grep .java)
