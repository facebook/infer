#!/bin/bash
# Wrapper around the opensource clang meant to work around various path or library
# issues occurring when one tries to substitute Apple's version of clang with
# a different version.
# The wrapper tries to mitigate version discrepancies in clang's fatal warnings.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

CLANG_COMPILER="${SCRIPT_DIR}/../../../facebook-clang-plugins/clang/install/bin/clang"

# WARNING: use at your own risk, not needed in most cases
# Path that points to clang internal headers to be replaced with
# path to infer's clang internal headers.
CLANG_INCLUDE_TO_REPLACE="${FCP_CLANG_INCLUDE_TO_REPLACE}"

CLANG_LIB_INCLUDE="${SCRIPT_DIR}/../../../facebook-clang-plugins/clang/install/lib/clang/4.0.0/include"

if [ "${0%++}" != "$0" ]; then XX="++"; else XX=""; fi

COMMAND=("${CLANG_COMPILER}${XX}")

# Remove command line options not supported by the opensource compiler or the plugins.
PREV=""
for X in "$@"
do
    if [   "$X" == "-fembed-bitcode-marker" \
        -o "$X" == "-fno-canonical-system-headers" ]; then
        continue
    elif [ "$X" == "armv7k" ] && [ "$PREV" == "-arch" ]; then
        # replace armv7k arch with armv7
        COMMAND+=("armv7")
    elif [ "$X" == "$CLANG_INCLUDE_TO_REPLACE" ] && [ "$PREV" == "-isystem" ]; then
        COMMAND+=("$CLANG_LIB_INCLUDE")
    else
        COMMAND+=("$X")
    fi
    PREV="$X"
done

# Never error on warnings. Clang is often more strict than Apple's version.
# These arguments are appended to override previous opposite settings.
# How it's done: surpress all the warnings, since there are no warnings,
# compiler can't elevate them to error level.
COMMAND+=("-Wno-everything")

COMMAND+=("-include")
COMMAND+=(${SCRIPT_DIR}/"global_defines.h")

"${COMMAND[@]}"
