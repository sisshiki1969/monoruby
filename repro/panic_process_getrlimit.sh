#!/bin/bash
# Process.getrlimit spec causes PANIC at builtins/array.rs:1788
# (unimplemented! in Array flatten for unhandled argument type)
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/process/getrlimit_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / panic)
