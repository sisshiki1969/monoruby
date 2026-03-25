#!/bin/bash
# IO#close spec hangs - IO.popen + ruby_cmd subprocess never terminates
cd /home/user
timeout 15 /home/user/monoruby/target/release/monoruby \
  /home/user/mspec/bin/mspec-run \
  /home/user/spec/core/io/close_spec.rb 2>&1
echo "EXIT: $?"
# Expected: exit 124 (timeout)
