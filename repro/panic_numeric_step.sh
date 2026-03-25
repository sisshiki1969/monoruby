#!/bin/bash
# Numeric#step spec causes PANIC at codegen/jitgen/state/slot.rs:1333
# JIT type state merge failure (unreachable in gen_bridge)
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/numeric/step_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / panic)
