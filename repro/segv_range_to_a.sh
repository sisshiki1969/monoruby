#!/bin/bash
# Range#to_a SEGV - occurs only through mspec (release build, JIT-related)
# Debug build does NOT reproduce.
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/range/to_a_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 139 (SEGV)
# Direct execution of Range#to_a works fine:
#   monoruby -e 'p (-5..5).to_a'  => OK
