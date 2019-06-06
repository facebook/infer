#!/bin/bash

# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -x

BUCK=buck
GENRULE_SUFFIX="_infer_capture"
BUCK_KIND_PATTERN="^(java|android)_library$"
INFER_BIN="${INFER_BIN:-infer}"

INFER_VERSION=$(${INFER_BIN} --version | head -1 | cut -f3 -d' ')

ROOT_TARGET="${1?Must specify a root target.}"
shift
QUERY="kind('${BUCK_KIND_PATTERN}', deps('${ROOT_TARGET}'))"
QUERY="attrfilter(labels, infer_enabled, ${QUERY})"

INFER_OUT="${1?Must specify an infer out location.}"
if [[ "$INFER_OUT" != /* ]] ; then
  echo "Must use absolute path for infer out location."
  exit 1
fi
shift

BUCK_CONFIG="--config infer.project_root=${PWD}"
BUCK_CONFIG="${BUCK_CONFIG} --config infer.infer_out=${INFER_OUT}"
BUCK_CONFIG="${BUCK_CONFIG} --config infer.infer_bin=${INFER_BIN}"
BUCK_CONFIG="${BUCK_CONFIG} --config infer.enabled=True"
BUCK_CONFIG="${BUCK_CONFIG} --config infer.version=${INFER_VERSION}"

# prepare infer-out, mainly for runstate
$INFER_BIN -o "${INFER_OUT}" > /dev/null 2>&1

TARGET_FILE=$(mktemp)
trap "{ rm -f $TARGET_FILE; }" EXIT

echo "Running buck query."
$BUCK query ${BUCK_CONFIG} "${QUERY}" | sed "s/\$/${GENRULE_SUFFIX}/" > "${TARGET_FILE}"

if [ -s "${TARGET_FILE}" ]
then
  echo "Found $(wc -l < ${TARGET_FILE})  targets."
else
  echo "Zero targets found!"
  exit 1
fi

echo 'Running genrule capture under buck.'
$BUCK build --no-cache ${BUCK_CONFIG} "@${TARGET_FILE}"

echo 'Running merge and analysis.'
$INFER_BIN analyze --genrule-master-mode -o "${INFER_OUT}" "$@"
