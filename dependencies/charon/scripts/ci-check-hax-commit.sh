#!/usr/bin/env bash
# Check that we're using a hax commit that's merged into main.

HAX_COMMIT="$(toml2json charon/Cargo.lock | jq -r \
    '.package[]
    | select(.name == "hax-frontend-exporter").source
    | capture("^git\\+https://github.com/(cryspen|AeneasVerif)/hax\\?branch=(?<branch>[a-z]+)#(?<commit>[a-f0-9]+)$")
    | select(.branch == "main")
    | .commit
    ')"
echo "This PR uses hax commit $HAX_COMMIT"

git clone https://github.com/AeneasVerif/hax
cd hax
HAX_MAIN="$(git rev-parse HEAD)"

if ! git merge-base --is-ancestor "$HAX_COMMIT" "$HAX_MAIN"; then
    echo "Error: commit $HAX_COMMIT is not merged into the main branch of AeneasVerif/hax."
    exit 1
fi
