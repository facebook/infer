#!/bin/bash

# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Given the arguments a `clang -cc1 ...` command, attaches a plugin to
# the clang command, then run our own clang with all the arguments
# (passing through filter_args_and_run_fcp_clang.sh) and pipe the
# output to InferClang.

#### Configuration ####
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BIN_DIR="${SCRIPT_DIR}/../../bin"
ETC_DIR="${SCRIPT_DIR}/../../etc"

# path to the wrapped clang compiler to invoke
CLANG_COMPILER="${SCRIPT_DIR}/filter_args_and_run_fcp_clang"
# extension of the file containing the clang cmd intercepted
CMD_FILE_EXT=".sh"
# extension of the file containing the output of the Infer Clang frontend
INFERCLANG_LOG_FILE_EXT=".astlog"
# path of the plugin to load in clang
PLUGIN_PATH="${SCRIPT_DIR}/../../../facebook-clang-plugins/libtooling/build/FacebookClangPlugin.dylib"
# name of the plugin to use
PLUGIN_NAME="BiniouASTExporter"
# output directory of the plugin
RESULTS_DIR="$FCP_RESULTS_DIR"
# this skips the creation of .o files
SYNTAX_ONLY="$FCP_RUN_SYNTAX_ONLY"
# extra arguments to pass during the execution of the infer frontend
INFER_FRONTEND_ARGS=($FCP_INFER_FRONTEND_ARGS)
# this fails the execution of clang if the frontend fails
REPORT_FRONTEND_FAILURE="$FCP_REPORT_FRONTEND_FAILURE"
# enable debug mode (to get more data saved to disk for future inspections)
DEBUG_MODE="$FCP_DEBUG_MODE"
# specify where is located Apple's clang
APPLE_CLANG="$FCP_APPLE_CLANG"
# whether to amend include search path with C++ model headers
INFER_CXX_MODELS="$FCP_INFER_CXX_MODELS"

# invariants that this script expects
if [ -z "$RESULTS_DIR" ]; then
    echo '$FCP_RESULTS_DIR with the name of the output directory not provided.' 1>&2
    exit 1
fi
if [ "$1" != "-cc1" ]; then
    echo "$0 expects to be run with -cc1" 1>&2
    exit 1
fi

# we know the first argument is "-cc1"
shift


# Functions
function get_option_argument {
    # retrieves the value passed to an argument of a clang command
    OPT="$1"
    shift
    while [ -n "$1" ] && [ "$1" != "$OPT" ]; do shift; done
    echo "$2"
}

function has_flag {
    # return if the given flag is part of the given command or not
    local FLAG="$1"
    shift
    while [ -n "$1" ] && [ "$1" != "$FLAG" ]; do shift; done
    [ -n "$1" ];
}

# Main
INPUT_ARGUMENTS=("$@")

# -cc1 has to be the first argument or clang will think it runs in driver mode
CLANG_CMD=("${CLANG_COMPILER}${XX}" "-cc1")

# It's important to place this option before other -isystem options.
if [ -n "$INFER_CXX_MODELS" ]; then
    CLANG_CMD+=("-isystem" "${SCRIPT_DIR}/../../models/cpp/include")
fi

# (t7400979) this is a workaround to avoid that clang crashes when the -fmodules flag
# and the YojsonASTExporter plugin are used. Since the -plugin argument disables
# the generation of .o files, we invoke apple clang again to generate the expected
# artifacts. This will keep xcodebuild plus all the sub-steps happy.
if [ -n "$APPLE_CLANG" ]; then
    ADD_PLUGIN_FLAG="-plugin"
else
    ADD_PLUGIN_FLAG="-add-plugin"
fi

CLANG_CMD+=(
    "-load"
    "${PLUGIN_PATH}"
    "$ADD_PLUGIN_FLAG"
    "${PLUGIN_NAME}"
    "-plugin-arg-${PLUGIN_NAME}"
    "-"
    "-plugin-arg-${PLUGIN_NAME}"
    "PREPEND_CURRENT_DIR=1")

if [ -n "$LLVM_MODE" ]; then
    CLANG_CMD+=("-o" "-" "-g" "-S" "-emit-llvm")
fi

# add the remaining arguments
CLANG_CMD+=("$@")

# the source file is at the end of the command, match it with the wanted extensions
SOURCE_FILENAME="${INPUT_ARGUMENTS[${#INPUT_ARGUMENTS[@]} - 1]}"

if ! [[ "$SOURCE_FILENAME" = /* ]]; then
    SOURCE_FILENAME="$(pwd)/$SOURCE_FILENAME"
fi

# add fsyntax-only to the end of arg list to override previous options
if [ -n "$SYNTAX_ONLY" ]; then
    CLANG_CMD+=("-fsyntax-only")
fi

if [ -n "$LLVM_MODE" ]; then
    INFER_FRONTEND_CMD=(
        "${BIN_DIR}/InferLLVM"
        "-c" "$SOURCE_FILENAME"
        "-results_dir" "$RESULTS_DIR"
        "${INFER_FRONTEND_ARGS[@]}")
    INFER_FRONTEND_LOG_FILE="/dev/stdout"
else
    LANGUAGE=$(get_option_argument "-x" "${INPUT_ARGUMENTS[@]}")
    if [ -n "$LANGUAGE" ]; then INFER_FRONTEND_ARGS+=("-x" "$LANGUAGE"); fi
    if has_flag "-fobjc-arc" "${INPUT_ARGUMENTS[@]}"; then
        INFER_FRONTEND_ARGS+=("-fobjc-arc");
    fi

    INFER_FRONTEND_CMD=(
        "${BIN_DIR}/InferClang"
        "-c" "$SOURCE_FILENAME"
        "-results_dir" "$RESULTS_DIR"
        "${INFER_FRONTEND_ARGS[@]}")

    if [ -n "$DEBUG_MODE" ]; then
        OBJECT_FILENAME="$(get_option_argument "-o" "${INPUT_ARGUMENTS[@]}")"
        # Emit the clang command with the extra args piped to InferClang
        echo "${CLANG_CMD[@]} " \
             "| tee ${OBJECT_FILENAME}.biniou " \
             "| ${INFER_FRONTEND_CMD[@]}" \
             > "${OBJECT_FILENAME}${CMD_FILE_EXT}"
        echo "bdump -x -d ${ETC_DIR}/clang_ast.dict -w '!!DUMMY!!' ${OBJECT_FILENAME}.biniou " \
             "> ${OBJECT_FILENAME}.bdump" \
             >> "${OBJECT_FILENAME}${CMD_FILE_EXT}"
        # Emit the InferClang cmd used to run the frontend
        INFER_FRONTEND_LOG_FILE="${OBJECT_FILENAME}${INFERCLANG_LOG_FILE_EXT}"
        echo "${INFER_FRONTEND_CMD[@]}" > "$INFER_FRONTEND_LOG_FILE"
    else
        INFER_FRONTEND_LOG_FILE="/dev/null"
    fi
fi

# run clang and pipe its output to InferClang/InferLLVM, or flush it in case the latter crashes
"${CLANG_CMD[@]}" | \
  ("${INFER_FRONTEND_CMD[@]}" || \
   { EC=$?; cat > /dev/null; exit $EC; }) \
  >> "$INFER_FRONTEND_LOG_FILE" 2>&1
STATUSES=("${PIPESTATUS[@]}")
STATUS="${STATUSES[0]}"
INFER_STATUS="${STATUSES[1]}"

# if clang fails, then fail, otherwise, fail with the frontend's exitcode if required
if [ "$STATUS" == 0 ] && [ -n "$REPORT_FRONTEND_FAILURE" ]; then
    STATUS="$INFER_STATUS"
fi

exit $STATUS
