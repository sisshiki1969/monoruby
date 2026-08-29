//! Integration coverage for *main script* semantics: the `DATA` constant
//! and `TOPLEVEL_BINDING`. Both are properties of how the main program is
//! executed (`Executor::exec_main_script` runs it inside the
//! TOPLEVEL_BINDING binding; a `__END__` marker opens `DATA` on the
//! script file), so they can only be exercised by spawning the real
//! binary — `run_test` runs its code in-process with a "." path.

use std::io::Write;
use std::process::Command;

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.arg("--disable-gems");
    cmd
}

fn write_temp(name: &str, body: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(name);
    let mut f = std::fs::File::create(&path).expect("create temp file");
    f.write_all(body.as_bytes()).expect("write temp file");
    path
}

fn stdout_of(cmd: &mut Command) -> String {
    let out = cmd.output().expect("failed to spawn monoruby");
    assert!(
        out.status.success(),
        "monoruby exited with {:?}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// `DATA` is a File on the main script positioned just past `__END__`;
/// `rewind` seeks to the head of the script itself. A required file's
/// `__END__` must not (re)define it.
#[test]
fn data_constant_reads_past_end_marker() {
    let lib = write_temp("monoruby_main_data_lib.rb", "__END__\nfrom lib\n");
    let script = write_temp(
        "monoruby_main_data.rb",
        "require_relative 'monoruby_main_data_lib'\n\
         print DATA.read\n\
         DATA.rewind\n\
         print DATA.gets\n\
         __END__\ndata body\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(
        out,
        "data body\nrequire_relative 'monoruby_main_data_lib'\n"
    );
    let _ = std::fs::remove_file(script);
    let _ = std::fs::remove_file(lib);
}

/// Without `__END__` in the main script, `DATA` is not defined — even
/// when a required file has one.
#[test]
fn data_constant_absent_without_end_marker() {
    let script = write_temp(
        "monoruby_main_nodata.rb",
        "puts Object.const_defined?(:DATA)\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "false\n");
    let _ = std::fs::remove_file(script);
}

/// TOPLEVEL_BINDING exposes exactly the main script's locals: empty
/// while a `-r` require runs, then the script's parse-time locals with
/// live values, merged with dynamically-set Binding variables.
#[test]
fn toplevel_binding_tracks_main_script_locals() {
    let lib = write_temp(
        "monoruby_main_tb_lib.rb",
        "p TOPLEVEL_BINDING.local_variables\n\
         TOPLEVEL_BINDING.local_variable_set(:from_lib, 40)\n",
    );
    let script = write_temp(
        "monoruby_main_tb.rb",
        "p TOPLEVEL_BINDING.local_variables.sort\n\
         p TOPLEVEL_BINDING.local_variable_get(:a)\n\
         a = 1\n\
         p TOPLEVEL_BINDING.local_variable_get(:a)\n\
         p TOPLEVEL_BINDING.local_variable_get(:from_lib) + eval('a', TOPLEVEL_BINDING) + 1\n",
    );
    let out = stdout_of(monoruby().arg("-r").arg(&lib).arg(&script));
    assert_eq!(out, "[]\n[:a, :from_lib]\nnil\n1\n42\n");
    let _ = std::fs::remove_file(script);
    let _ = std::fs::remove_file(lib);
}

/// A toplevel `return` — bare or from inside a block — terminates the
/// main script (no LocalJumpError), and `return <arg>` warns that the
/// argument is ignored without affecting the exit status.
#[test]
fn toplevel_return_terminates_main_script() {
    let script = write_temp(
        "monoruby_main_return.rb",
        "puts 'a'\n1.times { return }\nputs 'b'\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "a\n");
    let _ = std::fs::remove_file(script);

    let script = write_temp("monoruby_main_return_arg.rb", "return 3\n");
    let out = monoruby().arg(&script).output().expect("spawn");
    assert!(out.status.success(), "exit status must be 0, not 3");
    assert!(
        String::from_utf8_lossy(&out.stderr)
            .contains("warning: argument of top-level return is ignored"),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = std::fs::remove_file(script);
}

/// The main script runs on a plain (stack) frame, so its hot loops are
/// JIT-compilable — `TOPLEVEL_BINDING` is not built until something
/// reads it. A read from *other* code (here a required library, through
/// a dynamic `const_get` the compile-time scan cannot see) builds it
/// over the running script's frame, so it still exposes that script's
/// live locals.
#[test]
fn toplevel_binding_materializes_over_a_running_main_script() {
    let lib = write_temp(
        "monoruby_main_tb_lazy_lib.rb",
        "def peek\n\
        \x20 b = Object.const_get(:TOPLEVEL_BINDING)\n\
        \x20 [b.local_variables.sort, b.local_variable_get(:q)]\n\
         end\n",
    );
    let script = write_temp(
        "monoruby_main_tb_lazy.rb",
        &format!(
            "require_relative '{}'\n\
             q = 42\n\
             i = 0\n\
             i += 1 while i < 200\n\
             p peek\n\
             p eval('q + i', Object.const_get(:TOPLEVEL_BINDING))\n",
            lib.file_stem().unwrap().to_string_lossy()
        ),
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "[[:i, :q], 42]\n242\n");
    let _ = std::fs::remove_file(script);
    let _ = std::fs::remove_file(lib);
}

/// A script that names `TOPLEVEL_BINDING` itself runs inside the
/// binding, because the two places that read it there — a thread body
/// and an `at_exit` handler — do so where the script's own frame is not
/// reachable (another call chain; after the toplevel frame is gone).
/// Both must still see the script's locals.
#[test]
fn toplevel_binding_named_by_the_script_survives_thread_and_at_exit() {
    let script = write_temp(
        "monoruby_main_tb_named.rb",
        "z = 7\n\
         at_exit { p TOPLEVEL_BINDING.local_variable_get(:z) }\n\
         Thread.new { p TOPLEVEL_BINDING.local_variables.sort }.join\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "[:z]\n7\n");
    let _ = std::fs::remove_file(script);
}

/// Backtraces label the main script's own frames `<main>` (a required
/// file's toplevel stays `<top (required)>`), on either path.
#[test]
fn main_script_frames_are_labeled_main() {
    let script = write_temp(
        "monoruby_main_label.rb",
        "def boom = raise('x')\n\
         begin\n\
        \x20 boom\n\
         rescue => e\n\
        \x20 puts e.backtrace.map { |l| l.sub(/\\A.*:\\d+:in /, '') }\n\
         end\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "'Object#boom'\n'<main>'\n");
    let _ = std::fs::remove_file(script);

    // Same script, but naming TOPLEVEL_BINDING so it runs inside the
    // binding: the label must not change.
    let script = write_temp(
        "monoruby_main_label_bound.rb",
        "TOPLEVEL_BINDING\n\
         def boom = raise('x')\n\
         begin\n\
        \x20 boom\n\
         rescue => e\n\
        \x20 puts e.backtrace.map { |l| l.sub(/\\A.*:\\d+:in /, '') }\n\
         end\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "'Object#boom'\n'<main>'\n");
    let _ = std::fs::remove_file(script);
}

/// `TOPLEVEL_BINDING` is a *defined* constant before anything reads it:
/// it lists in `Object.constants`, answers `const_defined?`/`defined?`,
/// and is not an autoload.
#[test]
fn toplevel_binding_is_defined_before_it_is_built() {
    let script = write_temp(
        "monoruby_main_tb_defined.rb",
        "p Object.const_defined?(:TOPLEVEL_BINDING)\n\
         p Object.constants.include?(:TOPLEVEL_BINDING)\n\
         p defined?(TOPLEVEL_BINDING)\n\
         p Object.autoload?(:TOPLEVEL_BINDING)\n",
    );
    let out = stdout_of(monoruby().arg(&script));
    assert_eq!(out, "true\ntrue\n\"constant\"\nnil\n");
    let _ = std::fs::remove_file(script);
}
