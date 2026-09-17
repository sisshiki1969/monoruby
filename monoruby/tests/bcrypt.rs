extern crate monoruby;
use monoruby::tests::*;

// The bcrypt gem over monoruby's stand-in for bcrypt_ext.so
// (gem/bcrypt_ext.rb + src/builtins/bcrypt.rs on the bcrypt crate). Every
// case is compared against the host CRuby, which runs the gem's real C
// extension (crypt_blowfish), so these pin the stand-in's output byte for
// byte: same salt in, same hash out.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and bcrypt is an ordinary gem.

/// `Engine.hash_secret` with fixed salts: the four prefixes, cost
/// digits, the salt's last character normalized as crypt_blowfish does,
/// the 72-byte cut, the NUL-byte `ArgumentError`, non-ASCII secrets, and
/// the result's encoding. Plus what the gem raises for bad salts.
#[test]
fn bcrypt_hash_secret() {
    run_test_once(
        r##"
        require "rubygems"
        require "bcrypt"
        res = []
        salt = "$2a$04$abcdefghijklmnopqrstuu"
        res << BCrypt::Engine.hash_secret("secret", salt)
        res << BCrypt::Engine.hash_secret("secret", salt).encoding.name
        res << BCrypt::Engine.hash_secret("", salt)
        res << BCrypt::Engine.hash_secret("secret", "$2b$05$abcdefghijklmnopqrstuu")
        res << BCrypt::Engine.hash_secret("secret", "$2y$04$abcdefghijklmnopqrstuu")
        res << BCrypt::Engine.hash_secret("secret", "$2x$04$abcdefghijklmnopqrstuu")
        res << BCrypt::Engine.hash_secret("secret", "$2a$10$./A9Zz0123456789ABCDEF")
        # The salt's 22nd character carries only 2 significant bits.
        res << %w[e f g h].map { |c| BCrypt::Engine.hash_secret("s", "$2a$04$abcdefghijklmnopqrstu#{c}") }.uniq.size
        long = "x" * 80
        res << [BCrypt::Engine.hash_secret(long, salt) == BCrypt::Engine.hash_secret(long[0, 72], salt),
                BCrypt::Engine.hash_secret(long, salt) == BCrypt::Engine.hash_secret(long[0, 71], salt)]
        res << (begin; BCrypt::Engine.hash_secret("ab\0cd", salt); rescue => e; [e.class.name, e.message]; end)
        res << (begin; BCrypt::Engine.hash_secret("ab", salt + "\0"); rescue => e; [e.class.name, e.message]; end)
        res << BCrypt::Engine.hash_secret("pässwörd 日本", salt)
        res << BCrypt::Engine.hash_secret(12345, salt)
        probe = lambda { |&b| begin; b.call; rescue => e; [e.class.name, e.message]; end }
        res << probe.call { BCrypt::Engine.hash_secret("s", "bogus") }
        res << probe.call { BCrypt::Engine.hash_secret("s", "$2a$04$tooshort") }
        res << probe.call { BCrypt::Engine.hash_secret("s", nil) }
        res << probe.call { BCrypt::Engine.hash_secret(nil, salt) }
        res << [BCrypt::Engine.valid_salt?(salt), BCrypt::Engine.valid_salt?("$2a$04$abc"), BCrypt::Engine.valid_salt?("x")]
        res << [BCrypt::Engine.valid_secret?("x"), BCrypt::Engine.valid_secret?(nil)]
        res
        "##,
    );
}

/// `Engine.generate_salt` (random, so only its shape), and the
/// `BCrypt::Password` layer on top: `create` / `==` / `is_password?`,
/// `cost`, `version`, `salt`, `checksum`, `valid_hash?`, and the
/// `InvalidHash` / `InvalidCost` errors.
#[test]
fn bcrypt_password() {
    run_test_once(
        r##"
        require "rubygems"
        require "bcrypt"
        res = []
        s = BCrypt::Engine.generate_salt(4)
        res << [s.size, s[0, 7], s.encoding.name, !!(s =~ %r{\A\$2a\$04\$[./A-Za-z0-9]{22}\z})]
        res << [BCrypt::Engine.generate_salt(12)[0, 7], BCrypt::Engine.generate_salt[0, 4]]
        res << (BCrypt::Engine.generate_salt(4) != BCrypt::Engine.generate_salt(4))
        pw = BCrypt::Password.create("hunter2", cost: 4)
        res << [pw.class.name, pw.size, pw.cost, pw.version, pw.salt.size, pw.checksum.size, pw.encoding.name]
        res << [pw == "hunter2", pw == "hunter3", pw.is_password?("hunter2"), pw.is_password?("wrong")]
        res << [pw == pw.to_s, pw == BCrypt::Password.new(pw.to_s).to_s]
        again = BCrypt::Password.new(pw.to_s)
        res << [again == "hunter2", again.cost, again.salt == pw.salt, again.checksum == pw.checksum]
        fixed = BCrypt::Password.new("$2a$04$abcdefghijklmnopqrstuu2r9OfJnfCsdneAXAGHnS4UpFFP8WIrW")
        res << [fixed == "secret", fixed == "Secret", fixed.cost, fixed.salt, fixed.checksum, fixed.version]
        res << BCrypt::Password.valid_hash?("$2a$04$abcdefghijklmnopqrstuu2r9OfJnfCsdneAXAGHnS4UpFFP8WIrW")
        res << BCrypt::Password.valid_hash?("nope")
        probe = lambda { |&b| begin; b.call; rescue => e; [e.class.name, e.message]; end }
        res << probe.call { BCrypt::Password.new("nope") }
        res << probe.call { BCrypt::Password.create("x", cost: 3).cost }
        res << probe.call { BCrypt::Password.create("x", cost: 32) }
        res << BCrypt::Engine::DEFAULT_COST
        res << [BCrypt::Engine.cost, (BCrypt::Engine.cost = 4), BCrypt::Password.create("x").cost]
        # The SecureRandom-free path `Engine.__bc_salt` is private.
        res << [BCrypt::Engine.respond_to?(:__bc_salt), BCrypt::Engine.respond_to?(:__bc_crypt)]
        res
        "##,
    );
}
