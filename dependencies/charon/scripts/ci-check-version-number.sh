#!/usr/bin/env bash
# Compares this version with `main`, and checks that if `charon-ml` changed,
# then so did the version in `Cargo.toml`.
if git diff origin/main --quiet -- charon-ml/src/generated/Generated_*OfJson.ml; then
    echo '`charon-ml` was not changed in this PR; all good.'
else
    echo -n '`charon-ml` was changed in this PR '
    if git diff origin/main --quiet -- charon-ml/src/CharonVersion.ml; then
        echo 'but the charon version was not updated properly!'
        exit 1
    else
        echo 'and the charon version was updated properly.'
    fi
fi
