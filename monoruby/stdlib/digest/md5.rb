# frozen_string_literal: true
require 'digest'

module Digest
  class MD5 < Base
    ALGORITHM = 'md5'
    DIGEST_LENGTH = 16
    BLOCK_LENGTH = 64
  end
end
