#!/bin/bash
# Set#flatten spec causes stack overflow (recursive Set without cycle detection)
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/set/flatten_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 134 (SIGABRT / stack overflow)
