#!/bin/sh
# Build the documentation book (output: docs/book/).
#
# The "Design Documents" section of the book is sourced from ../doc at build
# time: this script copies doc/*.md and the diagrams into src/design/ (which
# is gitignored) and then runs `mdbook build`. Planning/handoff memos are
# excluded. If a new design document is added to doc/, list it in
# src/SUMMARY.md to give it a chapter.
set -eu
cd "$(dirname "$0")"

rm -rf src/design
mkdir -p src/design
cp ../doc/*.md src/design/
cp ../doc/*.svg ../doc/*.png src/design/
rm -f src/design/handoff_record_stream.md src/design/plan-activerecord.md

mdbook build
