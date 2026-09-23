# encoding_matrix — String / Encoding probes against CRuby

The scripts here exercise `String`, `Symbol`, `Regexp`, `IO` and
`Encoding::Converter` across encodings and print one row per case;
`expected/` holds the rows the pinned CRuby (`vendor/ruby-stdlib/.ruby-version`)
printed, and `bin/encoding-matrix` runs the same scripts on a monoruby
binary and diffs row by row. It is the measurement behind
`doc/encoding_api_plan_2026-09.md`, and the way an encoding PR shows
"N rows fixed, 0 regressed":

```sh
bin/encoding-matrix --out /tmp/before          # on the base commit
bin/encoding-matrix --base /tmp/before          # on the branch: fixed / regressed per script
bin/encoding-matrix --record                    # refresh expected/ (ruby on PATH must be the pin)
```

| script | rows | what |
|---|---|---|
| `strmatrix.rb` | 6,290 | ~150 String methods × ~40 inputs (UTF-8 / BINARY / US-ASCII / EUC-JP / Shift_JIS / UTF-16 / dummy encodings, valid and broken); each row is the result's bytes, encoding, `valid_encoding?`, `ascii_only?` and frozenness, or the exception. `ONLY_INPUT=<name>` runs one input, `SKIP="in\tcase|…"` skips rows — for isolating a crash. |
| `misc.rb`, `misc2.rb` | 95 + 96 | Everything that is not a single String method: literals and `eval`, `Symbol`, `format`, `Regexp` literals and matching per encoding, `Encoding` objects, `Marshal`, `Comparable`, `Hash` keys, … |
| `ioenc.rb` | 97 × 2 locales | `File` / `IO` / `StringIO` external and internal encodings, `LANG=` and `LANG=C.UTF-8` |
| `conv_cov.rb` | 10,507 | Every ordered pair of `Encoding.list`: does `Encoding::Converter.search_convpath` find a path |
| `hop2.rb` | 325 | `Encoding::Converter#primitive_convert` results and error details on two-hop paths |
| `lit/*.rb` | 1 each | Source-file encoding: magic comments, BOM, shebang, EUC-JP / Shift_JIS / US-ASCII sources, a bad `# encoding:`; the row is the program's whole stdout + stderr |

Rows are keyed by their leading tab-separated fields (the input and case
names), so a run that crashes part-way still compares the rows it printed.
A script's exit status is reported next to its counts.

The larger sweeps the plan mentions (`cjkall.rb` 288k rows, `sjp_full.rb`
44k, `isow3.rb` 13k) are not recorded here — their expected outputs would be
tens of megabytes — and are regenerated on demand.
