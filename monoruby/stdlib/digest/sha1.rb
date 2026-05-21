# frozen_string_literal: true
require 'digest'

module Digest
  class SHA1 < Base
    ALGORITHM = 'sha1'
    DIGEST_LENGTH = 20
    BLOCK_LENGTH = 64
  end
end
