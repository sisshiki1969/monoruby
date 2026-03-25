#!/bin/bash
# Range#count, #min, #max, #minmax specs hang
# Root cause: these methods are not implemented on Range.
# When called on endless ranges, fallback to Enumerable iteration
# causes infinite loop.
cd /home/user
for spec in count_spec min_spec max_spec minmax_spec; do
    echo "=== $spec ==="
    timeout 15 /home/user/monoruby/target/release/monoruby \
      /home/user/mspec/bin/mspec-run \
      /home/user/spec/core/range/${spec}.rb 2>&1
    echo "EXIT: $?"
done
# Expected: all exit 124 (timeout)
