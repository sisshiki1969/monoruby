#!/bin/bash
# String#% (modulo) spec causes PANIC at bytecodegen/expression.rs:451
# or iseq.rs:431 depending on context
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/string/modulo_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / panic)
