#!/bin/bash
# Running all array specs causes PANIC at iseq.rs:431
# (get_cache_map unwrap failure during JIT recompile)
# Does NOT reproduce with individual spec files - requires
# accumulated JIT state from running many specs sequentially.
cd /home/user
timeout 60 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/array 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / panic)
