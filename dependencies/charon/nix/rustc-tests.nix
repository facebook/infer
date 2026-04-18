{ bash
, charon
, coreutils
, fd
, fetchFromGitHub
, lib
, parallel
, pv
, runCommand
, rustToolchain
, writeScript
}:

let
  # The rustc commit we use to get the tests. We should update it every now and
  # then to match the version of rustc we're using.
  tests_commit = "94b49fd998d6723e0a9240a7cff5f9df37b84dd8";
  tests_hash = "sha256-gNaSgFKWIavLvtr9xuZRrOwzSExPT3+9obJ+sOyTFms=";

  rustc-test-suite = fetchFromGitHub {
    owner = "rust-lang";
    repo = "rust";
    rev = tests_commit;
    sha256 = tests_hash;
  };

  test_parsing_helpers = ''
    has_magic_comment() {
      # Checks for `// magic-comment` and `//@ magic-comment` instructions in files.
      grep -q "^//@\? \?$1" "$2"
    }

    has_feature() {
      # Checks for `#![feature(...)]`.
      grep -q "^#!.feature(.*$1.*)" "$2"
    }
  '';

  # Run charon on a single test file. This writes the charon output to
  # `<file>.rs.charon-output` and the exit status to `<file>.rs.charon-status`.
  run_rustc_test = writeScript "charon-run-rustc-test" ''
    #!${bash}/bin/bash
    FILE="$1"

    ${test_parsing_helpers}

    # TODO: revisions and edition would be good to support.
    if [ -e "$(dirname "$FILE")/compiletest-ignore-dir" ] \
      || [[ "$FILE" =~ (^|/)auxiliary($|/) ]] \
      || has_magic_comment 'ignore-test' "$FILE" \
      || has_magic_comment 'ignore-auxiliary' "$FILE" \
      || has_magic_comment 'known-bug' "$FILE" \
      || has_magic_comment 'only-' "$FILE" \
      || has_magic_comment 'needs-asm-support' "$FILE" \
      || has_magic_comment 'revisions' "$FILE" \
      || has_magic_comment 'dont-check-compiler-stderr' "$FILE" \
      || has_magic_comment 'stderr-per-bitwidth' "$FILE" \
      || has_magic_comment 'aux-build' "$FILE" \
      || has_magic_comment 'aux-crate' "$FILE" \
      || has_magic_comment 'rustc-env' "$FILE" \
      || has_magic_comment 'compile-flags' "$FILE" \
      || has_magic_comment 'edition' "$FILE" \
      ; then
        result="unsupported-build-settings"
    elif has_feature 'generic_const_exprs' "$FILE" \
      || has_feature 'adt_const_params' "$FILE" \
      || has_feature 'min_generic_const_args' "$FILE" \
      || has_feature 'effects' "$FILE" \
      || has_feature 'transmutability' "$FILE" \
      || has_feature 'loop_match' "$FILE" \
      || has_feature 'default_type_parameter_fallback' "$FILE" \
      ; then
        result="unsupported-feature"
    else
        ${coreutils}/bin/timeout 10s ${charon}/bin/charon rustc --dest-file "$FILE.llbc" -- "$FILE" > "$FILE.charon-output" 2>&1
        result=$?
    fi
    echo -n $result > "$FILE.charon-status"
  '';

  # Runs charon on the whole rustc ui test suite. This returns the tests
  # directory with a bunch of `<file>.rs.charon-output` and
  # `<file>.rs.charon-status` files, see `run_rustc_test`.
  run_rustc_tests = runCommand "charon-run-rustc-tests"
    {
      src = rustc-test-suite;
      buildInputs = [ rustToolchain parallel pv fd ];
    } ''
    mkdir $out
    cp -r $src/tests/ui/* $out
    chmod -R u+w $out
    cd $out

    SIZE="$(fd -e rs | wc -l)"
    echo "Running $SIZE tests..."
    fd -e rs \
        | parallel ${run_rustc_test} \
        | pv -l -s "$SIZE"
  '';

  # Report the status of a single file.
  analyze_test_output = writeScript "charon-analyze-test-output" ''
    #!${bash}/bin/bash
    FILE="$1"

    ${test_parsing_helpers}

    status="$(cat "$FILE.charon-status")"
    if echo "$status" | grep -q '^unsupported'; then
        result="⊘ $status"
    elif false \
      || [[ "$FILE" == "test-results/cfg/assume-incomplete-release/auxiliary/ver-cfg-rel.rs" ]]\
      || [[ "$FILE" == "test-results/macros/auxiliary/macro-comma-support.rs" ]]\
      || [[ "$FILE" == "test-results/meta/no_std-extern-libc.rs" ]]\
      || [[ "$FILE" == "test-results/parser/issues/auxiliary/issue-21146-inc.rs" ]]\
      ; then
        result="⊘ unsupported-build-settings"
    elif grep -q 'error.E0601' "$FILE.charon-output"; then
        # That's the "`main` not found" error we get on auxiliary files.
        result="⊘ unsupported-build-settings"
    elif grep -q 'error.E0463' "$FILE.charon-output"; then
        # "Can't find crate" error.
        result="⊘ unsupported-build-settings"
    elif grep -q 'error.E0583' "$FILE.charon-output"; then
        # "file not found for module" error.
        result="⊘ unsupported-build-settings"
    else
        if [ $status -eq 0 ]; then
            got="success"
        elif [ $status -eq 124 ]; then
            got="stack overflow/timeout"
        elif grep -q 'error: internal compiler error' "$FILE.charon-output" \
           || grep -q 'thread .rustc. .* panicked' "$FILE.charon-output" \
            ; then
            got="internal compiler error"
        elif [ $status -eq 101 ] || [ $status -eq 255 ]; then
            if grep -q 'fatal runtime error: stack overflow' "$FILE.charon-output"; then
                got="stack overflow/timeout"
            else
                got="panic"
            fi
        elif [ $status -eq 2 ]; then
            got="failure in rustc"
        else
            got="failure in charon"
        fi

        result="❌"
        extras=""
        if false \
          || has_magic_comment 'run-pass' "$FILE" \
          || has_magic_comment 'check-pass' "$FILE" \
          || ! [ -f ${"$"}{FILE%.rs}.stderr ] \
          ; then
            expected="success"
            if [[ $got == "success" ]]; then
                if [ -e "$FILE.llbc" ]; then
                    result="✅"
                    if grep -q 'The extraction generated .* warnings' "$FILE.charon-output"; then
                        extras="warnings"
                    else
                        extras="no warnings"
                    fi
                else
                    result="❌"
                    extras="without llbc output"
                fi
            fi
            # if [ -e "$FILE.llbc" ] && ! [[ $got == "success" ]]; then
            #     # If we have a failure and an llbc file, the failure happened while serializing.
            #     got="$got while serializing"
            # fi
        else
            expected="failure"
            if [[ $got == "failure in rustc" ]]; then
                result="✅"
            elif ! [[ $got == "success" ]]; then
                got="other failure"
            fi
        fi


        if ! [[ $extras == "" ]]; then
            extras=" ($extras)"
        fi
        result="$(printf "$result expected: %-18s  got: $got$extras" "$expected")"
    fi

    echo "$FILE: $result"
  '';

  # Adds a `charon-results` file that records
  # `success|expected-failure|failure|panic|timeout` for each file we
  # processed.
  analyze_test_outputs = runCommand "charon-analyze-test-outputs"
    {
      src = run_rustc_tests;
      buildInputs = [ parallel pv fd ];
    } ''
    mkdir $out
    chmod -R u+w $out
    cd $out
    ln -s $src test-results

    SIZE="$(fd --follow -e rs | wc -l)"
    echo "Running $SIZE tests..."
    fd --follow -e rs \
        | parallel ${analyze_test_output} \
        | pv -l -s "$SIZE" \
        > charon-results

    cat charon-results | cut -d':' -f 2- | sort | uniq -c > charon-summary

    function gather_errors() {
        expected="$1"
        got="$2"
        echo '<details><summary>'"❌ expected: $expected; got: $got"'</summary>'
        grep "expected: $expected.*got: $got" charon-results | cut -d':' -f1 | while read f; do
            echo
            echo '`'"$f"'`:'
            head -3 "$f.charon-output"
        done || true
        echo
        echo '</details>'
    }
    gather_errors "success" "failure in rustc" >> charon-grouped-results
    gather_errors "success" "internal compiler error" >> charon-grouped-results
    gather_errors "success" "panic" >> charon-grouped-results
  '';

in
{
  inherit rustc-test-suite;
  rustc-tests = analyze_test_outputs;
}
