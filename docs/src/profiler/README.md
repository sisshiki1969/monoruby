# Profiler example pages

Generated files, shown by `../profiler.md` (the "Built-in Profiler" chapter;
`optcarrot.html` is embedded there in an `<iframe>`). mdBook copies them into
the book as they are; this README is not part of the book.

Regenerate them when the viewer (`monoruby/src/prof/viewer.html`) or the
report changes, with a `--features prof` release build as `monoruby` on
`PATH`. The command line is shown at the top of each page, so run from these
directories with relative paths:

```sh
# in ../optcarrot (https://github.com/mame/optcarrot)
MONORUBY_PROF=all MONORUBY_PROF_INLINE=1 MONORUBY_PROF_OUT=optcarrot \
    monoruby bin/optcarrot -b --frames 600 examples/Lan_Master.nes
#   -> optcarrot.html, optcarrot.txt

# in benchmark/
MONORUBY_PROF=cpu MONORUBY_PROF_OUT=aobench-cpu monoruby app_aobench.rb
#   -> aobench-cpu.html, aobench-cpu.txt
```

The chapter quotes numbers from both runs (the excerpt of `aobench-cpu.txt`,
the sizes, what the first 300 ms of optcarrot look like, the commit at the
bottom); update them together with the files.
