# Run on monoruby (the gem installed through its C API layer) and on the
# host CRuby by tests/capi_gems.rs; both must print the same.
gem "bcrypt", ENV.fetch("CAPI_GEM_VERSION")
require "bcrypt"
puts $LOADED_FEATURES.grep(/\.(so|bundle)\z/).map { |f| File.basename(f, ".*") }.grep(/\A(bcrypt_ext|msgpack|parser|generator)\z/).sort.inspect
salt = "$2a$04$abcdefghijklmnopqrstuu"
h = BCrypt::Engine.hash_secret("secret", salt)
puts h, h.encoding
pw = BCrypt::Password.new(h)
p pw == "secret", pw == "wrong", pw.cost, pw.version, pw.salt, pw.checksum
p BCrypt::Engine.valid_salt?(salt), BCrypt::Engine.valid_salt?("nope"), BCrypt::Engine.valid_secret?("x"), BCrypt::Engine.valid_secret?(nil)
p BCrypt::Password.create("hello", cost: 4) == "hello"
p BCrypt::Password.create("hello", cost: 4).cost
begin; BCrypt::Engine.hash_secret("x", "bad"); rescue => e; p e.class, e.message; end
begin; BCrypt::Engine.hash_secret(nil, salt); rescue => e; p e.class; end
p BCrypt::Engine.hash_secret("", salt)
p BCrypt::Engine.hash_secret("a" * 100, salt)
p BCrypt::Engine.hash_secret("日本語のパスワード", salt)
begin; p BCrypt::Engine.hash_secret("with\x00nul", salt); rescue ArgumentError => e; p e.class, e.message; end
p BCrypt::Engine.send(:__bc_salt, "$2a$", 5, "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f".b)
p BCrypt::Engine.send(:__bc_salt, "$2b$", 12, "\xff" * 16)
p BCrypt::Engine.send(:__bc_crypt, "secret", salt)
p BCrypt::Engine.send(:__bc_crypt, "secret", "$2b$05$" + "x" * 22)
s2 = BCrypt::Engine.generate_salt(5)
p s2.size, s2[0, 7], BCrypt::Engine.valid_salt?(s2)
p BCrypt::Engine.calibrate(10) >= 4
p BCrypt::Password.valid_hash?(h), BCrypt::Password.valid_hash?("x")
p BCrypt::Password.new(h).is_password?("secret")
p BCrypt::Engine.cost
