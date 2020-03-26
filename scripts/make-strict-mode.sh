#!/bin/bash

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# USAGE
#
# 1. Follow instructions in https://source.android.com/setup/build/downloading to download
#    the AOSP source and in https://source.android.com/setup/build/building to get proprietary
#    binaries.  You will need lots of space (at least 100Gb).  Let <android-root> be its root as
#    an *absolute path*.
#
# 2. Replace <android-root>/prebuilts/jdk/jdk9/<your OS>/bin/javac with the following script.
#
#    #!/bin/bash
#    infer -q --capture --continue --starvation-only --no-starvation \
#      --project-root <android-root> --results-dir <android-root>/infer-out -- \
#      /usr/local/bin/javac "$@"
#
#    Here, my local installation of java is in /usr/local/, change accordingly.  I used a
#    Java *8* installation without problems, YMMV.
#
# 3. From <android-root> do
#
#    $ . build/envsetup.sh
#    $ export TEMPORARY_DISABLE_PATH_RESTRICTIONS=true
#    $ cd libcore/ojluni
#    $ mm -j1 javac-check
#
#    ... and wait.  It took me ~22h.
#
# 4. From <android-root> run
#
#    $ infer analyze --starvation-only --dev-android-strict-mode
#
# 5. From <android-root> run this script, capturing stdout.
#
#    $ <infer-root>/scripts/make-strict-mode.sh > \
#        <infer-root>/infer/src/concurrency/StrictModeModels.ml
#
# 6. You may need to adapt the optional ~actuals_pred argument for methods in the above ML file.
#    The aim is to avoid false positives when there is an overloaded method with different
#    signatures and it so happens that one of the versions makes a violation when another does not.
#    Recompile Infer.


SOURCE_FILES=$(grep "error:" infer-out/report.txt | cut -f1 -d: | sort -u | grep -v test )
MATCHERS=""

cat <<EOF
(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open ConcurrencyModels

EOF

for SOURCE_FILE in $SOURCE_FILES ; do
  PACKAGE=$(grep -E "^package " $SOURCE_FILE | cut -f2 -d' ' | cut -f1 -d\;)

  if [[ $PACKAGE != android.* ]] && [[ $PACKAGE != androidx.* ]] && [[ $PACKAGE != java.* ]] ; then
    continue
  fi

  BASENAME=$(basename $SOURCE_FILE )
  CLASS=${BASENAME%.*}

  if ! grep -q -E "public.*class.* ${CLASS}" $SOURCE_FILE ; then
    continue
  fi

  HIDE=$(grep -B 2 -E "public.*class.* ${CLASS}" $SOURCE_FILE | grep '@hide')
  if [ ! -z "$HIDE" ] ; then
    continue
  fi

  FULLCLASSNAME="${PACKAGE}.${CLASS}"
  METHOD_REXP="^  Method \`.* $CLASS\."
  METHODS=$(grep -E "$METHOD_REXP" infer-out/report.txt | cut -f2 -d. | cut -f1 -d\` | sort -u)

  if [ -z "$METHODS" ] ; then
    continue
  fi

  HEADER=""
  MATCHER="is_${CLASS}_method"

  for METHOD in $METHODS; do
    METHODNAME=$(echo $METHOD | cut -f1 -d\( )

    if ! grep -q -E "public.*${METHODNAME}" $SOURCE_FILE ; then
      continue
    fi

    if [ -z "$HEADER" ] ; then
      echo "(* $SOURCE_FILE *)"
      echo "let ${MATCHER} ="
      echo "  is_call_of_class \"${FULLCLASSNAME}\""
      echo "    ["
      HEADER=true
    fi

    echo "      \"${METHODNAME}\"; (* $METHOD *)"
  done

  if [ ! -z "$HEADER" ] ; then
    echo "    ]"
    echo '  |> Staged.unstage'
    echo
    MATCHERS="$MATCHERS $MATCHER"
  fi
done

echo
echo "let is_strict_mode_violation ="
echo "  let matchers = ["
for M in $MATCHERS ; do
  echo "    $M ;"
done
echo "    ]"
echo "  in"
echo "  fun tenv pn actuals -> List.exists matchers ~f:(fun m -> m tenv pn actuals)"
