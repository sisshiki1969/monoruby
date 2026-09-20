#!/bin/bash
# SessionStart hook — bring a fresh Claude Code on the web container up to the
# state the test suite needs. Mirrors "Development Environment Setup" §2 in
# CLAUDE.md and the gem list in .github/workflows/rust.yml; keep the three in
# sync. Safe to re-run: every step is a no-op once it has been done.
set -euo pipefail

# Local checkouts have their own toolchains. Only the prebuilt web image starts
# from the state this script knows how to fix up, so leave everything else be.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
    exit 0
fi

repo="${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export RBENV_ROOT="${RBENV_ROOT:-/opt/rbenv}"
export PATH="$RBENV_ROOT/bin:$PATH"

# ---------------------------------------------------------------- Rust ------
# The pinned nightly, with the components CI's toolchain action brings along.
toolchain="$(sed -n 's/^[[:space:]]*channel[[:space:]]*=[[:space:]]*"\(.*\)"/\1/p' \
    "$repo/rust-toolchain.toml")"
echo "==> rust: $toolchain"
rustup toolchain install "$toolchain" -c rustfmt -c clippy --no-self-update

# ---------------------------------------------------------------- CRuby -----
# The reference Ruby must match the vendored pin exactly: several expected
# outputs in the suite are version-specific.
pin="$(cat "$repo/monoruby/vendor/ruby-stdlib/.ruby-version")"
echo "==> cruby: $pin"

# The image's ruby-build plugin predates the pin, so it has no definition to
# build from. Move it to its newest tag when the definition is missing.
rb="$RBENV_ROOT/plugins/ruby-build"
if [ ! -f "$rb/share/ruby-build/$pin" ]; then
    echo "--> updating ruby-build (no definition for $pin)"
    git -C "$rb" fetch --depth 50 --tags origin
    git -C "$rb" checkout --detach "$(git -C "$rb" tag --sort=-v:refname | head -1)"
fi

# ~6 minutes on a cold container; `-s` makes an already-built version free.
# The image carries the build deps (libssl/libyaml/zlib/readline/ffi/gdbm-dev),
# so there is no apt step.
MAKE_OPTS="-j$(nproc)" RUBY_CONFIGURE_OPTS=--disable-install-doc \
    rbenv install -s "$pin"
rbenv global "$pin"
rbenv rehash

# ---------------------------------------------------------------- PATH ------
# `rbenv global` alone is not enough: the image wires its baked-in ruby ahead of
# rbenv in two independent places, and both have to be undone.

# Login shells: /etc/profile.d/ruby.sh prepends /opt/ruby-<ver>/bin and sorts
# after rbenv.sh, so it wins. Add a later-sorting file that puts the shims first.
if [ -w /etc/profile.d ]; then
    cat > /etc/profile.d/zz-rbenv-shims.sh <<'PROFILE'
# Put rbenv shims ahead of the baked-in /opt/ruby-*/bin entries so that
# `rbenv global` decides which ruby is on PATH.
if [ -d "${RBENV_ROOT:-/opt/rbenv}/shims" ]; then
    PATH="$(echo "$PATH" | sed -E 's#(^|:)/opt/ruby-[^:]*/bin(:|$)#\2#g; s#^:##; s#:$##')"
    export PATH="${RBENV_ROOT:-/opt/rbenv}/shims:$PATH"
fi
PROFILE
fi

# Non-login, non-interactive shells — what the agent's individual tool calls get
# — never read profile.d at all; they find /usr/local/bin/ruby, a symlink into
# the baked-in tree. Repoint those at the shims so `rbenv global` is the one
# switch that decides which ruby everything sees.
for c in ruby gem irb bundle; do
    if [ -e "$RBENV_ROOT/shims/$c" ] && [ -w /usr/local/bin ]; then
        ln -sfn "$RBENV_ROOT/shims/$c" "/usr/local/bin/$c"
    fi
done

# And this session's own shells, whichever kind they turn out to be.
if [ -n "${CLAUDE_ENV_FILE:-}" ] && ! grep -qs 'RBENV_ROOT' "$CLAUDE_ENV_FILE"; then
    {
        echo "export RBENV_ROOT=\"$RBENV_ROOT\""
        echo "export PATH=\"$RBENV_ROOT/shims:\$PATH\""
    } >> "$CLAUDE_ENV_FILE"
fi

export PATH="$RBENV_ROOT/shims:$PATH"
hash -r

# ---------------------------------------------------------------- gems ------
# The gems the integration tests pin monoruby's Ruby replacements against, at
# the versions .github/workflows/rust.yml installs. A test whose gem is missing
# skips, or fails its whole file with LoadError, so keep this in sync with CI.
# --conservative leaves an already-satisfied requirement alone.
echo "==> gems"
gem install --no-document --conservative \
    bigdecimal bcrypt msgpack yajl-ruby strptime cool.io zstd-ruby:1.5.7.0 \
    markly nokogiri erubi hexapdf chunky_png \
    rubocop:1.79.1 rubocop-performance:1.25.0 rubocop-rails:2.32.0

# monoruby caches the probed host Ruby and its gem paths; drop the cache so the
# next run re-probes instead of describing a Ruby this script just replaced.
rm -f "$HOME/.monoruby/library_path" "$HOME/.monoruby/gem_path" \
      "$HOME/.monoruby/probed_ruby"

# ---------------------------------------------------------------- check -----
echo "==> ready"
ruby -v
test "$(ruby -e 'print RUBY_VERSION')" = "$pin" \
    || { echo "ruby on PATH is not the pinned $pin" >&2; exit 1; }
