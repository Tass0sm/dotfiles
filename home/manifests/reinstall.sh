#!/usr/bin/env sh

for manifest in ./*.scm; do
    name=$(basename $manifest .scm)
    profile="$GUIX_EXTRA_PROFILES/$name/$name"
    guix package --profile="$profile" --manifest="$manifest"
done
