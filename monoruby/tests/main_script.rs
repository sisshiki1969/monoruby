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
