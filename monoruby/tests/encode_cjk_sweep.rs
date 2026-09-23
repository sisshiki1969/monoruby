extern crate monoruby;
use monoruby::tests::*;

// #1544 counted the BMP scalars the Chinese / Korean encoders wrote
// differently from CRuby's tables (14,753 across EUC-KR, Big5-HKSCS,
// GBK, Big5 and GB18030), and #1559 the GB12345 converter that did
// not exist. #1564 gave the CJK encodings CRuby's cell tables and
// #1520 the GB12345 converter over them; these sweeps pin every
// scalar of the BMP, and the supplementary planes where the
// encodings reach them, so a table regression shows as one digest.

/// Every BMP scalar into each encoding, digested.
#[test]
fn every_bmp_scalar_encodes_as_cruby_does() {
    run_test_once(
        r##"
        require "digest"
        %w[EUC-KR CP949 Big5 Big5-HKSCS Big5-UAO CP950 CP951 GBK GB2312 GB18030 GB12345].map do |e|
          d = Digest::SHA256.new
          n = 0
          (0x80..0xFFFF).each do |cp|
            next if (0xD800..0xDFFF).cover?(cp)
            r = begin
              b = cp.chr("UTF-8").encode(e)
              n += 1
              b.bytes
            rescue Encoding::UndefinedConversionError
              :undef
            end
            d << "#{cp}=#{r.inspect};"
          end
          [e, n, d.hexdigest]
        end
        "##,
    );
}

/// The supplementary planes, where GB18030 has every scalar and the
/// Big5 variants their HKSCS / UAO cells, digested.
#[test]
fn every_supplementary_scalar_encodes_as_cruby_does() {
    run_test_once(
        r##"
        require "digest"
        %w[GB18030 Big5-HKSCS Big5-UAO GBK EUC-KR].map do |e|
          d = Digest::SHA256.new
          n = 0
          (0x10000..0x2FFFF).each do |cp|
            r = begin
              b = cp.chr("UTF-8").encode(e)
              n += 1
              b.bytes
            rescue Encoding::UndefinedConversionError
              :undef
            end
            d << "#{cp}=#{r.inspect};"
          end
          [e, n, d.hexdigest]
        end
        "##,
    );
}

/// The way back: every two-byte cell of each encoding, digested — and
/// GB12345's converter in both directions, which #1559 found absent.
#[test]
fn every_cell_decodes_as_cruby_does() {
    run_test_once(
        r##"
        require "digest"
        res = %w[EUC-KR CP949 Big5 Big5-HKSCS Big5-UAO CP950 CP951 GBK GB2312 GB18030 GB12345].map do |e|
          d = Digest::SHA256.new
          n = 0
          (0x81..0xFE).each do |b1|
            (0x40..0xFE).each do |b2|
              s = [b1, b2].pack("C*").force_encoding(e)
              r = begin
                u = s.encode("UTF-8")
                n += 1
                u.codepoints
              rescue Encoding::UndefinedConversionError
                :undef
              rescue Encoding::InvalidByteSequenceError
                :invalid
              end
              d << "#{b1},#{b2}=#{r.inspect};"
            end
          end
          [e, n, d.hexdigest]
        end
        res << "一".encode("GB12345").bytes
        res << [0xD2, 0xBB].pack("C*").force_encoding("GB12345").encode("UTF-8")
        res << "國".encode("GB12345").bytes
        res << (0x80..0xFFFF).count { |cp| next false if (0xD800..0xDFFF).cover?(cp); (cp.chr("UTF-8").encode("GB12345"); true) rescue false }
        res << Encoding::Converter.new("UTF-8", "GB12345").convpath.map { |p| p.map(&:to_s) }
        res
        "##,
    );
}
