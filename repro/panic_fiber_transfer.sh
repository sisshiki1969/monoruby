#!/bin/bash
# Fiber#transfer spec causes PANIC at function.rs:963 (as_iseq unreachable)
# RValue::debug -> RValue::to_s -> MonorubyErr::method_not_found path
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/fiber/transfer_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / panic)
