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
  tests_commit = "920d95eaf23d7eb6b415d09868e4f793024fa604";
  tests_hash = "sha256-sAOLzfvjKMi6oeS6EkZ8h5NHF2e6sNqeuq4yni2Pts4=";

  rustc-test-suite = fetchFromGitHub {
    owner = "rust-lang";
    repo = "rust";
    rev = tests_commit;
    sha256 = tests_hash;
  };

  # The commit that corresponds to our nightly pin, for when we want to update the pinned commit.
  toolchain_commit = runCommand "get-rustc-commit" { } ''
    # This is sad but I don't know a better way.
    cat ${rustToolchain}/share/doc/rust/html/version_info.html \
      | grep 'github.com' \
      | sed 's#.*"https://github.com/rust-lang/rust/commit/\([^"]*\)".*#\1#' \
      > $out
  '';

  # Run charon on a single test file. This writes the charon output to
  # `<file>.rs.charon-output` and the exit status to `<file>.rs.charon-status`.
  run_rustc_test = writeScript "charon-run-rustc-test" ''
    #!${bash}/bin/bash
    FILE="$1"

    has_magic_comment() {
      # Checks for `// magic-comment` and `//@ magic-comment` instructions in files.
      grep -q "^//@\? \?$1" "$2"
    }

    has_feature() {
      # Checks for `#![feature(...)]`.
      grep -q "^#!.feature(.*$1.*)" "$2"
    }

    # TODO: revisions and edition would be good to support.
    if [ -e "$(dirname "$FILE")/compiletest-ignore-dir" ] \
      || has_magic_comment 'ignore-test' "$FILE" \
      || has_magic_comment 'known-bug' "$FILE" \
      || has_magic_comment 'only-' "$FILE"\
      || has_magic_comment 'needs-asm-support' "$FILE"\
      || has_magic_comment 'revisions' "$FILE" \
      || has_magic_comment 'dont-check-compiler-stderr' "$FILE"\
      || has_magic_comment 'stderr-per-bitwidth' "$FILE"\
      || has_magic_comment 'aux-build' "$FILE" \
      || has_magic_comment 'aux-crate' "$FILE" \
      || has_magic_comment 'rustc-env' "$FILE"\
      || has_magic_comment 'compile-flags' "$FILE" \
      || has_magic_comment 'edition' "$FILE"\
      ; then
        result="unsupported-build-settings"
    elif has_feature 'generic_const_exprs' "$FILE" \
      || has_feature 'adt_const_params' "$FILE" \
      || has_feature 'effects' "$FILE" \
      || has_feature 'transmutability' "$FILE" \
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
    else
        if [ -f ${"$"}{FILE%.rs}.stderr ]; then
            expected="failure in rustc"
        else
            expected="success"
        fi
        if [ $status -eq 0 ]; then
            got="success"
        elif [ $status -eq 124 ]; then
            got="timeout"
        elif [ $status -eq 101 ] || [ $status -eq 255 ]; then
            if grep -q 'fatal runtime error: stack overflow' "$FILE.charon-output"; then
                got="stack overflow"
            else
                got="panic"
            fi
        else
            got="failure"
            if grep -q 'error.E9999' "$FILE.charon-output"; then
                got="$got in hax frontend"
            elif [ $status -eq 2 ]; then
                got="$got in rustc"
            else
                # This won't happen since we don't pass `--error-on-warnings`.
                got="$got in charon"
            fi
        fi

        if [[ $expected == $got ]]; then
            status="✅"
        else
            status="❌"
        fi
        extras=""
        if [[ $expected == "success" ]]; then
            if [ -e "$FILE.llbc" ]; then
                if ! [[ $got == "success" ]]; then
                    # If we have a failure and an llbc file, the failure happened while serializing.
                    got="$got while serializing"
                else
                    extras="with llbc output and "
                fi
                if grep -q 'The extraction generated .* warnings' "$FILE.charon-output"; then
                    extras="$extras""warnings"
                else
                    extras="$extras""no warnings"
                fi
            else
                extras="without llbc output"
                status="❌"
            fi
        fi
        if ! [[ $extras == "" ]]; then
            extras=" ($extras)"
        fi
        result="$(printf "$status expected: %-18s  got: $got$extras" "$expected")"
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
            echo "<details><summary>$f</summary>"
            echo
            echo '```text'
            cat "$f.charon-output"
            echo '```'
            echo
            echo '</details>'
        done || true
        echo
        echo '</details>'
    }
    gather_errors "success" "failure in rustc" >> charon-grouped-results
    gather_errors "success" "failure in hax" >> charon-grouped-results
    gather_errors "success" "stack overflow" >> charon-grouped-results
  '';

in
{
  inherit toolchain_commit rustc-test-suite;
  rustc-tests = analyze_test_outputs;
}
