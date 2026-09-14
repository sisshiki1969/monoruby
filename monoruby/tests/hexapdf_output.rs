extern crate monoruby;
use monoruby::tests::ruby_path;
use std::process::Command;

// hexapdf (a pure-Ruby PDF library) compresses every content stream with
// `Zlib::Deflate.new(9, MAX_WBITS, 6)`, so a document it writes is only
// identical to CRuby's when our deflate produces zlib's exact bit stream.
// Render the same document with the monoruby binary and with the host
// CRuby and compare what they print: the PDF's size and CRC-32, which for
// a document of this size leaves no room for a different stream. (ruby-bench's
// hexapdf benchmark checks the same thing through a fixed expected size.)
//
// Both sides run as real processes with rubygems enabled — the in-process
// `run_test` interpreters and the oracle's CRuby run with gems disabled.
// Needs the `hexapdf` gem on the host Ruby (CI installs it); skipped, not
// failed, when it is absent so a bare checkout still tests green.

const SCRIPT: &str = r#"
require "hexapdf"
require "stringio"
require "zlib"
text = (1..120).map do |i|
  "Paragraph #{i}. The quick brown fox jumps over the lazy dog; " \
  "pack my box with five dozen liquor jugs. Sphinx of black quartz, judge my vow. " \
  "Value #{i * 7919 % 1000} appears here, and the line wraps at fifty points."
end.join("\n\n")
composer = HexaPDF::Composer.new(page_size: [0, 0, 50, 1000], margin: 0)
composer.text(text, font_features: {kern: false}, font: "Times", font_size: 10,
              last_line_gap: true, line_spacing: {type: :fixed, value: 11.16})
composer.document.trailer[:ID] = ['monoruby', 'monoruby']
io = StringIO.new("".b)
composer.write(io, update_fields: false)
pdf = io.string
p [pdf.bytesize, Zlib.crc32(pdf), pdf.start_with?("%PDF-"), composer.document.pages.count]
"#;

fn run(mut cmd: Command) -> String {
    let out = cmd
        .env("LC_ALL", "C.UTF-8")
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .output()
        .expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} exited with {:?}\nstderr: {}",
        cmd.get_program(),
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn hexapdf_document_bytes_match_cruby() {
    let probe = Command::new(ruby_path())
        .args(["-e", "require 'hexapdf'; print HexaPDF::VERSION"])
        .output();
    match probe {
        Ok(out) if out.status.success() => {
            eprintln!("hexapdf {}", String::from_utf8_lossy(&out.stdout));
        }
        _ => {
            eprintln!("skipped: the hexapdf gem is not installed for the host ruby");
            return;
        }
    }
    let mut ruby = Command::new(ruby_path());
    ruby.args(["-E", "UTF-8", "-e", SCRIPT]);
    let expected = run(ruby);
    let mut mono = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    mono.args(["-e", SCRIPT]);
    let got = run(mono);
    eprintln!("ruby:     {expected}\nmonoruby: {got}");
    assert_eq!(expected, got);
    // A document this size cannot be summarised this way by accident.
    let size: usize = got.trim_start_matches('[').split(',').next().unwrap().trim().parse().unwrap();
    assert!(size > 10_000, "unexpectedly small PDF: {got}");
}
