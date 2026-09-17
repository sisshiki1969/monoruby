# Stand-in for the bcrypt gem's C extension (`bcrypt_ext.so`): the two
# entry points `BCrypt::Engine` calls, on the native `bcrypt` in
# `src/builtins/bcrypt.rs` (the `bcrypt` crate, whose output is
# crypt_blowfish's byte for byte). The gem's Ruby half (`bcrypt/engine`,
# `bcrypt/password`) is the installed gem's own and makes both private.
module BCrypt
  class Engine
    def self.__bc_salt(prefix, cost, random_bytes)
      String.__bcrypt_salt(prefix, cost, random_bytes)
    end

    def self.__bc_crypt(secret, salt)
      String.__bcrypt_crypt(secret, salt)
    end
  end
end
