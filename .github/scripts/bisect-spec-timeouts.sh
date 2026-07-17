#!/usr/bin/env bash
# Identify the culprit example(s) inside spec files that exceeded the
# core monitor's per-file budget, and register them as `fails` tags.
#
# For each `category,file` row of the input timeouts.csv, every example
# of the file is run individually under a hard deadline. An example is a
# culprit when it either times out (a genuine hang: exit 124/137) or
# takes longer than SLOW_SECS by itself (a near-budget burner like a
# ~55s blocking wait — no single hang, but the file cannot fit the
# budget with it). Culprits are appended (deduplicated) to the repo's
# tag files, which both rubyspec-stats and the monitor exclude.
#
# A file with no individual culprit (purely cumulative slowness) is
# reported as such and left untagged — that calls for a budget change,
# not an exclusion.
#
# Usage:
#   bisect-spec-timeouts.sh TIMEOUTS_CSV SPEC_DIR MSPEC RUBY_CMD TAGS_DIR OUT_CSV
#
#   TIMEOUTS_CSV  monitor output: "category,file" rows (with header)
#   SPEC_DIR      ruby/spec checkout (file paths are relative to it)
#   MSPEC         path to mspec launcher (bin/mspec)
#   RUBY_CMD      the monoruby binary to test
#   TAGS_DIR      tag tree root to update (e.g. <repo>/spec/tags)
#   OUT_CSV       report written here: category,file,example,seconds,verdict

set -uo pipefail

TIMEOUTS_CSV=$1
SPEC_DIR=$2
MSPEC=$3
RUBY_CMD=$4
TAGS_DIR=$5
OUT_CSV=$6

# Per-example hard deadline (mirrors the monitor's per-file budget) and
# the "too slow to share a file budget" threshold.
EX_BUDGET=${EX_BUDGET:-60}
SLOW_SECS=${SLOW_SECS:-30}
# Wall-clock cap per file: a pathological file (every example hanging)
# must not eat the whole job. Whatever was found so far is kept.
FILE_CAP_SECS=${FILE_CAP_SECS:-900}

echo "category,file,example,seconds,verdict" > "$OUT_CSV"

# A pre-rendered markdown twin of OUT_CSV (descriptions may contain
# commas, so consumers should not re-parse the CSV with naive splits).
OUT_MD="${OUT_CSV%.csv}.md"
{
  echo "| Category | File | Example | Seconds | Verdict |"
  echo "| --- | --- | --- | ---: | --- |"
} > "$OUT_MD"

record () {
  # record CATEGORY FILE DESC SECONDS VERDICT
  echo "$1,$2,\"$3\",$4,$5" >> "$OUT_CSV"
  echo "| $1 | \`$2\` | $3 | $4 | $5 |" >> "$OUT_MD"
}

# Full example descriptions, one per line, via a dry run. specdoc prints
# the full joined describe context as a header line and each example as
# "- <it>"; a tag needs "<header> <it>". The dry run executes no example
# bodies but does load the file, so it gets a deadline too.
list_examples () {
  local file=$1
  timeout -k 5 60 "$MSPEC" run --dry-run -f s "$file" -t "$RUBY_CMD" 2>/dev/null \
    | awk '
        /^- /            { if (ctx != "") print ctx " " substr($0, 3); next }
        /^[[:space:]]*$/ { next }
                         { ctx = $0 }
      '
}

n_tagged=0

while IFS=, read -r cat file; do
  [ "$cat" = "category" ] && continue
  [ -z "${file:-}" ] && continue

  echo "== bisecting $file"
  examples=$(cd "$SPEC_DIR" && list_examples "$file")
  if [ -z "$examples" ]; then
    record "$cat" "$file" "" 0 dry-run-failed
    echo "   dry run produced no examples (file-level hang?) — needs manual attention"
    continue
  fi

  # spec/tags/<category-path>/<name>_tags.txt, mirroring mspec's
  # default tags_patterns derivation.
  tagfile="$TAGS_DIR/$(dirname "$file")/$(basename "$file" _spec.rb)_tags.txt"

  file_start=$SECONDS
  found_any=0
  while IFS= read -r desc; do
    [ -z "$desc" ] && continue
    if [ $((SECONDS - file_start)) -ge "$FILE_CAP_SECS" ]; then
      record "$cat" "$file" "" $((SECONDS - file_start)) file-cap-reached
      echo "   per-file bisect cap reached; remaining examples unchecked"
      break
    fi
    ex_start=$SECONDS
    (cd "$SPEC_DIR" && timeout -k 5 "$EX_BUDGET" \
      "$MSPEC" run "$file" -e "$desc" -t "$RUBY_CMD" > /dev/null 2>&1)
    rc=$?
    dur=$((SECONDS - ex_start))
    verdict=""
    if [ $rc -eq 124 ] || [ $rc -eq 137 ]; then
      verdict="hang"
    elif [ "$dur" -ge "$SLOW_SECS" ]; then
      verdict="slow"
    fi
    if [ -n "$verdict" ]; then
      found_any=1
      record "$cat" "$file" "$desc" "$dur" "$verdict"
      echo "   $verdict (${dur}s): $desc"
      mkdir -p "$(dirname "$tagfile")"
      if ! grep -qxF "fails:$desc" "$tagfile" 2>/dev/null; then
        echo "fails:$desc" >> "$tagfile"
        n_tagged=$((n_tagged + 1))
      fi
    fi
  done <<< "$examples"

  if [ "$found_any" -eq 0 ]; then
    record "$cat" "$file" "" $((SECONDS - file_start)) cumulative-only
    echo "   no individual culprit — cumulative slowness; not tagged"
  fi
done < "$TIMEOUTS_CSV"

echo "new tags added: $n_tagged"
echo "n_tagged=$n_tagged" >> "${GITHUB_OUTPUT:-/dev/null}"
