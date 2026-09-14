//! ruby-bench programs whose output is a deterministic function of their
//! inputs, run under the `monoruby` binary and under the host CRuby (both
//! with rubygems) and compared line for line. `bin/ruby-bench-diff` does
//! the same in one shell payload; these run inside `cargo test` so a
//! regression is pinned to one benchmark and one process.
//!
//! Each program prints a small fingerprint — sizes and CRC-32s of the
//! rendered template / image / message bytes, or the `inspect` of a loaded
//! document — rather than the whole output. Every test needs the
//! ruby-bench checkout at `$RUBY_BENCH` or `../ruby-bench` (CI clones it)
//! and skips otherwise; the gem-backed ones also skip when the host lacks
//! the gem (CI installs `erubi`, `chunky_png` and the rubocop gems).

extern crate monoruby;
use monoruby::tests::ruby_path;
use std::path::PathBuf;
use std::process::Command;

/// `benchmarks/` of the ruby-bench checkout, if there is one.
fn ruby_bench() -> Option<PathBuf> {
    let root = match std::env::var_os("RUBY_BENCH") {
        Some(p) => PathBuf::from(p),
        None => PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../ruby-bench"),
    };
    let dir = root.join("benchmarks");
    dir.is_dir().then_some(dir)
}

fn gem_available(name: &str) -> bool {
    Command::new(ruby_path())
        .args(["-e", &format!("require '{name}'")])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn run(mut cmd: Command, cwd: &PathBuf) -> String {
    let out = cmd
        .current_dir(cwd)
        .env("LC_ALL", "C.UTF-8")
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .output()
        .expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} exited with {:?} in {}\nstderr: {}",
        cmd.get_program(),
        out.status,
        cwd.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Run `script` in `benchmarks/<bench>` under both and compare.
fn compare(bench: &str, gems: &[&str], script: &str) {
    let Some(benchmarks) = ruby_bench() else {
        eprintln!("skipped: no ruby-bench checkout (set RUBY_BENCH or clone ../ruby-bench)");
        return;
    };
    let cwd = benchmarks.join(bench);
    assert!(cwd.is_dir(), "{} is not in the ruby-bench checkout", cwd.display());
    for gem in gems {
        if !gem_available(gem) {
            eprintln!("skipped: the {gem} gem is not installed for the host ruby");
            return;
        }
    }
    let mut ruby = Command::new(ruby_path());
    ruby.args(["-E", "UTF-8", "-e", script]);
    let expected = run(ruby, &cwd);
    let mut mono = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    mono.args(["-e", script]);
    let got = run(mono, &cwd);
    eprintln!("ruby:\n{expected}\nmonoruby:\n{got}");
    assert!(!expected.is_empty(), "{bench}: the program printed nothing");
    assert_eq!(expected, got, "{bench}: output differs from CRuby");
}

#[test]
fn erubi_template_renders_like_cruby() {
    // The gem-server index template over real gem specs: the generated
    // Ruby source and the rendered HTML (166563 bytes on both).
    compare(
        "erubi",
        &["erubi"],
        r##"
require "json"
require "erubi"
require "zlib"
src = Erubi::Engine.new(File.read("simple_template.erb")).src
eval <<RUBY
# frozen_string_literal: true
class ErbRenderer
  def initialize(values); @values = values; end
  def run_erb; #{src}; end
end
RUBY
values = JSON.load(File.read("gem_specs.json"))
out = ErbRenderer.new(values).run_erb
p [src.size, Zlib.crc32(src), out.size, Zlib.crc32(out), out.encoding.name]
"##,
    );
}

#[test]
fn etanni_template_renders_like_cruby() {
    // The same template in the Etanni dialect (pure Ruby: `eval` of a
    // heredoc-spliced Proc and `instance_eval`).
    compare(
        "etanni",
        &[],
        r##"
require "json"
require "zlib"
class Etanni
  SEPARATOR = "E69t116A65n110N78i105S83e101P80a97R82a97T84o111R82"
  CHOMP = "<<#{SEPARATOR}.chomp!"
  START = "\n_out_ << #{CHOMP}\n"
  STOP = "\n#{SEPARATOR}\n"
  REPLACEMENT = "#{STOP}\\1#{START}"
  def initialize(template, filename = '<Etanni>')
    temp = template.strip
    temp.gsub!(/<\?r\s+(.*?)\s+\?>/m, REPLACEMENT)
    @compiled = eval("Proc.new{ _out_ = [#{CHOMP}]\n#{temp}#{STOP}_out_.join }", nil, filename)
  end
  def result(instance)
    instance.instance_eval(&@compiled)
  end
end
@values = JSON.load(File.read("gem_specs.json"))
out = Etanni.new(File.read("simple_template.etanni")).result(self)
p [out.size, Zlib.crc32(out), out.encoding.name]
"##,
    );
}

#[test]
fn chunky_png_encodes_like_cruby() {
    // Every encoding the benchmark exercises: the PNG bytes depend on the
    // filter heuristics *and* on zlib producing the same deflate stream.
    compare(
        "chunky-png",
        &["chunky_png"],
        r##"
require "chunky_png"
require "zlib"
image = ChunkyPNG::Image.new(240, 180, ChunkyPNG::Color::TRANSPARENT)
image[10, 20] = ChunkyPNG::Color.rgba(255,   0,   0, 255)
image[50, 87] = ChunkyPNG::Color.rgba(  0, 255,   0, 255)
image[33, 99] = ChunkyPNG::Color.rgba(  0,   0, 255, 255)
60.times { |i| image[i * 4 % 240, (i * 7) % 180] = ChunkyPNG::Color.rgba(i * 4, 255 - i, i * 3 % 256, 200) }
sig = ->(s) { [s.bytesize, Zlib.crc32(s)] }
r = {}
[:no_compression, :fast_rgba, :fast_rgb, :good_compression, :best_compression].each do |preset|
  r[preset] = sig.(image.to_blob(preset))
end
r[:truecolor] = sig.(image.to_blob(color_mode: ChunkyPNG::COLOR_TRUECOLOR))
r[:truecolor_alpha] = sig.(image.to_blob(color_mode: ChunkyPNG::COLOR_TRUECOLOR_ALPHA))
r[:indexed] = sig.(image.to_blob(color_mode: ChunkyPNG::COLOR_INDEXED))
r[:interlaced] = sig.(image.to_blob(interlaced: true))
r[:rgba_stream] = sig.(image.to_rgba_stream)
r[:rgb_stream] = sig.(image.to_rgb_stream)
back = ChunkyPNG::Image.from_blob(image.to_blob(:best_compression))
r[:roundtrip] = [back.width, back.height, back == image]
r.each { |k, v| puts "#{k}: #{v.inspect}" }
"##,
    );
}

#[test]
fn protoboeuf_decodes_and_encodes_like_cruby() {
    // The generated pure-Ruby protobuf codec over the recorded parking-lot
    // messages: decode every message, re-encode it, and fingerprint both.
    compare(
        "protoboeuf-encode",
        &[],
        r##"
require "zlib"
load File.expand_path("benchmark_pb.rb")
bins = Marshal.load(File.binread("encoded_msgs.bin"))
lots = bins.map { |bin| ProtoBoeuf::ParkingLot.decode(bin) }
encoded = lots.map { |lot| ProtoBoeuf::ParkingLot.encode(lot) }
p [bins.size, bins.sum(&:bytesize), encoded.sum(&:bytesize), Zlib.crc32(encoded.join)]
again = encoded.map { |e| ProtoBoeuf::ParkingLot.encode(ProtoBoeuf::ParkingLot.decode(e)) }
p [encoded == bins, again == encoded]
"##,
    );
}

#[test]
fn blurhash_encodes_like_cruby() {
    // Pure-Ruby blurhash of the sample image: floating-point heavy, so the
    // hash string itself is the fingerprint.
    compare(
        "blurhash",
        &[],
        r##"
src = File.read("benchmark.rb")
src = src[0, src.index('require_relative "../../harness/loader"')] if src.include?('require_relative "../../harness/loader"')
eval src
pixels = File.binread("test.bin").bytes
p [pixels.size, Blurhash.encode_rb(204, 204, pixels), Blurhash.encode_rb(204, 204, pixels, x_comp: 3, y_comp: 3)]
"##,
    );
}

#[test]
fn psych_loads_like_cruby() {
    // The three YAML documents of psych-load (the benchmark body is
    // `Psych.load` of each), loaded and inspected; then dumped again and
    // re-loaded, so the emitter's text and the round trip are checked too.
    compare(
        "psych-load",
        &[],
        r##"
require "psych"
require "zlib"
Dir["yaml/*.yaml"].sort.each do |path|
  y = Psych.load(File.read(path))
  s = y.inspect
  d = Psych.dump(y)
  again = Psych.load(d)
  puts "#{File.basename(path)}: #{y.class} #{s.size} #{Zlib.crc32(s)} dump #{d.bytesize} #{Zlib.crc32(d)} round-trip #{again == y}"
end
"##,
    );
}

#[test]
fn rubocop_autocorrects_like_cruby() {
    // The rubocop benchmark: `RuboCop::Runner` with `--autocorrect` over
    // `fixture.rb` fed through the `stdin` option (the Ruby LSP's way),
    // under the benchmark's `.rubocop.yml` (rubocop-performance and
    // rubocop-rails plugins). Compared: the run's result, every offense
    // (cop, position, severity, corrected?) and the corrected source.
    // Result caching is off so both processes really inspect the file.
    compare(
        "rubocop",
        &["rubocop", "rubocop-performance", "rubocop-rails"],
        r##"
require "stringio"
require "rubocop"
require "zlib"
$offenses = []
class CollectFormatter < RuboCop::Formatter::BaseFormatter
  def file_finished(file, offenses)
    $offenses.concat(offenses.map { |o| [o.cop_name, o.line, o.column, o.severity.name, o.corrected?] })
  end
end
opts = RuboCop::Options.new.parse(["--stderr", "--force-exclusion", "--format", "CollectFormatter", "--raise-cop-error", "--autocorrect", "--cache", "false"]).first
path = File.expand_path("fixture.rb")
contents = File.read(path)
opts[:stdin] = contents
runner = RuboCop::Runner.new(opts, RuboCop::ConfigStore.new)
ok = runner.run([path])
corrected = opts[:stdin]
p [ok, $offenses.size, corrected.size, Zlib.crc32(corrected), corrected == contents]
$offenses.each { |o| p o }
"##,
    );
}
