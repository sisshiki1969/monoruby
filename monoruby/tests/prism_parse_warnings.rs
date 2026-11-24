//! prism's own parse warnings (#2126), each printed as CRuby 4.0.9
//! prints it and at the level CRuby prints it at.
//!
//! prism reports thirty kinds of warning (`PM_WARN_*`); monoruby used
//! to forward nine of them. The expectations below are CRuby's output
//! for the same script under `-w` (every warning) and without it (the
//! default-level ones only).

use std::path::PathBuf;
use std::process::Command;

fn monoruby(args: &[&str]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    cmd.env_remove("RUBYOPT").arg("--disable=gems").args(args);
    cmd
}

fn stderr(mut cmd: Command) -> String {
    let out = cmd.output().expect("failed to spawn monoruby");
    assert!(out.status.success(), "{out:?}");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

fn monoruby_stderr(args: &[&str]) -> String {
    stderr(monoruby(args))
}

/// A script in the temp directory, removed when dropped.
struct Script(PathBuf);

impl Script {
    fn new(name: &str, source: &str) -> Self {
        let path = std::env::temp_dir().join(format!("i2126_{name}_{}.rb", std::process::id()));
        std::fs::write(&path, source).unwrap();
        Script(path)
    }

    fn path(&self) -> &str {
        self.0.to_str().unwrap()
    }

    fn lines(&self, warnings: &[(u32, &str)]) -> Vec<String> {
        warnings
            .iter()
            .map(|(line, msg)| format!("{}:{line}: warning: {msg}", self.path()))
            .collect()
    }
}

impl Drop for Script {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}

/// `source` warns `verbose` under `-w` and `default` at the default
/// level and under `-W1` (each `(line, message)`, in order), running
/// and under `-c`; nothing under `-W0`.
fn check(name: &str, source: &str, verbose: &[(u32, &str)], default: &[(u32, &str)]) {
    let script = Script::new(name, source);
    let file = script.path();
    for (flags, expected) in [
        (&["-w"][..], verbose),
        (&[][..], default),
        (&["-W1"][..], default),
        (&["-W0"][..], &[][..]),
    ] {
        let err = monoruby_stderr(&[flags, &[file]].concat());
        assert_eq!(
            err.lines().collect::<Vec<_>>(),
            script.lines(expected),
            "{flags:?}: {err}"
        );
    }
    for (flag, expected) in [("-wc", verbose), ("-c", default)] {
        let out = monoruby(&[flag, file]).output().unwrap();
        assert_eq!(String::from_utf8_lossy(&out.stdout), "Syntax OK\n");
        let err = String::from_utf8_lossy(&out.stderr).into_owned();
        assert_eq!(
            err.lines().collect::<Vec<_>>(),
            script.lines(expected),
            "{flag}: {err}"
        );
    }
}

const BINARY: &str = "after local variable or literal is interpreted as binary operator even \
                      though it seems like";
const FIRST_ARG: &str = "ambiguous first argument; put parentheses or a space even after";

#[test]
fn the_script_of_the_issue() {
    let binary = format!("'+' {BINARY} unary operator");
    check(
        "issue",
        "def m1\n  return 1\n  2\nend\nx = 1e1020\na = 1\np(a +1) rescue 0\n\
         p(1 < a >= 0) rescue 0\nif 1 then end\nif a\n  3\n  end\np x\n",
        &[
            (3, "statement not reached"),
            (5, "Float 1e1020 out of range"),
            (7, &binary),
            (8, "comparison '>=' after comparison"),
            (9, "literal in condition"),
            (12, "mismatched indentations at 'end' with 'if' at 10"),
        ],
        &[],
    );
}

#[test]
fn ambiguous_operators_and_first_arguments() {
    // After a local variable an operator is binary, whatever the
    // spacing suggests; after a method name it starts the first
    // argument. Either reading is announced under `-w`.
    let binary = |op: &str, seems: &str| format!("'{op}' {BINARY} {seems}");
    let expected = [
        (5, binary("+", "unary operator")),
        (6, binary("-", "unary operator")),
        (7, binary("*", "argument prefix")),
        (8, binary("**", "argument prefix")),
        (9, binary("&", "argument prefix")),
        (10, binary("/", "regexp literal")),
        (11, binary("%", "string literal")),
        (12, binary("<<", "here document")),
        (13, format!("{FIRST_ARG} `-` operator")),
        (14, format!("{FIRST_ARG} `+` operator")),
        (
            15,
            "ambiguous `*` has been interpreted as an argument prefix".to_string(),
        ),
        (
            16,
            "ambiguous `**` has been interpreted as an argument prefix".to_string(),
        ),
        (
            17,
            "ambiguous `&` has been interpreted as an argument prefix".to_string(),
        ),
        (
            18,
            "ambiguous `/`; wrap regexp in parentheses or add a space after `/` operator"
                .to_string(),
        ),
        (19, format!("{FIRST_ARG} `-` operator")),
        (20, "possibly useless use of - in void context".to_string()),
    ];
    let expected: Vec<(u32, &str)> = expected.iter().map(|(l, m)| (*l, m.as_str())).collect();
    check(
        "ambiguous",
        "def m(*, **, &b) = 1\na = 1\nb = proc {}\nh = {}\n\
         m(a +1)\nm(a -1)\nm(a *1)\nm(a **1)\nm(a &1)\nm(a /1)\nm(a %1)\nm(a <<1)\n\
         m -1\nm +1\nm *[a]\nm **h\nm &b\nm /1/\nm -a\nm - 1\nm(-1)\nm (1)\n",
        &expected,
        &[],
    );
}

#[test]
fn literals_in_a_condition() {
    // A String or a plain Regexp literal warns at the default level;
    // a number, a Symbol, `__LINE__` or an interpolated Regexp under
    // `-w` only. "regex literal in …" is therefore one message at two
    // levels (lines 7 / 8, and the two ends of the flip-flop of line 24).
    check(
        "condition",
        "a = 1\nif 1 then end\nif 1.5 then end\nif \"s\" then end\nif \"s#{a}\" then end\n\
         if :s then end\nif /r/ then end\nif /r#{a}/ then end\nif __FILE__ then end\n\
         if __LINE__ then end\nif nil then end\nif true then end\nif (1) then end\n\
         if a && 1 then end\nif !1 then end\nunless 1 then end\nwhile 1 do break end\n\
         p 1 if 2\np(1 ? 2 : 3)\nif (1..2) then end\nif (1.5..a) then end\n\
         if (\"a\"..\"b\") then end\nif (a..:b) then end\nif (/a/../b#{a}/) then end\n\
         if (a = 1) then end\n",
        &[
            (2, "literal in condition"),
            (3, "literal in condition"),
            (4, "string literal in condition"),
            (5, "string literal in condition"),
            (6, "symbol literal in condition"),
            (7, "regex literal in condition"),
            (8, "regex literal in condition"),
            (9, "string literal in condition"),
            (10, "literal in condition"),
            (13, "literal in condition"),
            (14, "literal in condition"),
            (16, "literal in condition"),
            (17, "literal in condition"),
            (18, "literal in condition"),
            (19, "literal in condition"),
            (20, "integer literal in flip-flop"),
            (20, "integer literal in flip-flop"),
            (21, "literal in flip-flop"),
            (22, "string literal in flip-flop"),
            (22, "string literal in flip-flop"),
            (23, "symbol literal in flip-flop"),
            (24, "regex literal in flip-flop"),
            (24, "regex literal in flip-flop"),
            (25, "found '= literal' in conditional, should be =="),
        ],
        &[
            (4, "string literal in condition"),
            (5, "string literal in condition"),
            (7, "regex literal in condition"),
            (9, "string literal in condition"),
            (20, "integer literal in flip-flop"),
            (20, "integer literal in flip-flop"),
            (22, "string literal in flip-flop"),
            (22, "string literal in flip-flop"),
            (24, "regex literal in flip-flop"),
            (25, "found '= literal' in conditional, should be =="),
        ],
    );
}

#[test]
fn statements_after_a_jump() {
    // The statement right after `return` / `next` / `break` / `redo` /
    // `retry`, and only that one; not after `raise` or a conditional
    // `return`.
    check(
        "unreachable",
        "def m1\n  return 1\n  2\nend\ndef m2\n  [1].each do\n    next\n    3\n  end\n  \
         [1].each do\n    break\n    4\n  end\n  while false\n    redo\n    5\n  end\n  \
         begin\n    1\n  rescue\n    retry\n    6\n  end\n  raise \"x\"\n  7\nend\n\
         def m3\n  return if m1\n  8\nend\ndef m4 = (return 1; 2)\n\
         def m5\n  return\n  9\n  10\nend\n",
        &[
            (3, "statement not reached"),
            (8, "statement not reached"),
            (12, "statement not reached"),
            (16, "statement not reached"),
            (22, "statement not reached"),
            (31, "statement not reached"),
            (34, "statement not reached"),
            (34, "possibly useless use of a literal in void context"),
        ],
        &[],
    );
}

#[test]
fn mismatched_indentations() {
    let at = |word: &str, with: &str, line: u32| {
        format!("mismatched indentations at '{word}' with '{with}' at {line}")
    };
    let expected = [
        (4, at("end", "if", 2)),
        (6, at("end", "while", 5)),
        (8, at("end", "def", 7)),
        (10, at("end", "class", 9)),
        (12, at("end", "begin", 11)),
        (15, at("end", "case", 13)),
        (18, at("else", "if", 16)),
        (20, at("end", "else", 18)),
        (23, at("rescue", "begin", 21)),
        (25, at("ensure", "begin", 21)),
        (
            26,
            "possibly useless use of a literal in void context".to_string(),
        ),
        (36, at("end", "->", 35)),
        (32, "assigned but unused variable - x".to_string()),
    ];
    let expected: Vec<(u32, &str)> = expected.iter().map(|(l, m)| (*l, m.as_str())).collect();
    // A block's `end` / `}` and the `end` of an `if` that is the value
    // of an assignment are not held to their opening line's column.
    check(
        "indent",
        "a = 1\nif a\n  3\n  end\nwhile false\n end\ndef m\n    end\nclass C\n end\n\
         begin\n end\ncase a\nwhen 1\n  end\nif a\n  1\n else\n  2\nend\n\
         begin\n  1\n rescue\n  2\n ensure\n  3\nend\n[1].each do |x|\n    end\n\
         [1].each { |x|\n    }\nx = if a\n  1\nend\n-> do\n end\n",
        &expected,
        &[],
    );
    // The `warn_indent` magic comment switches the check, from where
    // it stands.
    check(
        "indent_off",
        "# warn_indent: false\na = 1\nif a\n  2\n    end\n# warn_indent: true\n\
         if a\n  2\n    end\n",
        &[(9, "mismatched indentations at 'end' with 'if' at 7")],
        &[],
    );
}

#[test]
fn magic_comments() {
    // A magic comment prism knows with a value it does not, and one
    // that only counts on a line of its own (or before any token).
    check(
        "magic",
        "# frozen_string_literal: foo\n# shareable_constant_value: bar\n# warn_indent: baz\n\
         # encoding: utf-8\n# typed: true\nx = 1 # shareable_constant_value: literal\n\
         p x # frozen_string_literal: true\n",
        &[
            (1, "invalid value for frozen_string_literal: foo"),
            (2, "invalid value for shareable_constant_value: bar"),
            (3, "invalid value for warn_indent: baz"),
            (
                6,
                "'shareable_constant_value' is ignored unless in comment-only line",
            ),
            (7, "'frozen_string_literal' is ignored after any tokens"),
        ],
        &[],
    );
}

#[test]
fn comparison_chains_float_range_and_keywords_at_eol() {
    check(
        "verbose_misc",
        "a = 1\np(1 < a < 3) rescue 0\np(1 <= a > 3) rescue 0\np((1 < a) == true)\n\
         x = 1e400\ny = -1.0e400\nz = 123456789012345678901234567890e400\nw = 1e-400\n\
         p x, y, z, w\nif\n  a\n  1\nelsif\n  a\n  2\nend\nunless\n  a\nend\n",
        &[
            (2, "comparison '<' after comparison"),
            (3, "comparison '>' after comparison"),
            (5, "Float 1e400 out of range"),
            (6, "Float 1.0e400 out of range"),
            (7, "Float 12345678901234567890... out of range"),
            (10, "`if` at the end of line without an expression"),
            (13, "`elsif` at the end of line without an expression"),
        ],
        &[],
    );
}

#[test]
fn warnings_at_the_default_level() {
    // An endless range closing a line, a space or a tab as the
    // character of a control / meta escape, and a `\r` that does not
    // end its line (a `\r\n` line ending is fine, and so is a `\r`
    // inside a string) all warn without `-w`.
    let expected = [
        (1, "... at EOL, should be parenthesized?"),
        (7, "invalid character syntax; use \\C-\\s"),
        (7, "invalid character syntax; use \\M-\\s"),
        (7, "invalid character syntax; use \\C-\\s"),
        (7, "invalid character syntax; use \\M-\\C-\\s"),
        (7, "invalid character syntax; use \\M-\\t"),
        (7, "invalid character syntax; use \\t"),
        (
            8,
            "encountered \\r in middle of line, treated as a mere space",
        ),
    ];
    let mut verbose = expected.to_vec();
    verbose.push((10, "possibly useless use of a literal in void context"));
    check(
        "default_level",
        "a = 1...\n2\nb = (1...\n3)\nc = [1...]\np a, b, c\n\
         p \"\\C- \", \"\\M- \", \"\\c \", \"\\M-\\C- \", \"\\M-\t\", \"\\C-\t\", \"\\C-a\"\n\
         d = 1 \r + 2\np d\r\n\"x\ry\"\n",
        &verbose,
        &expected,
    );
}

const SHEBANG: &str = "shebang line ending with \\r may cause problems";

#[test]
fn a_shebang_line_ending_in_a_carriage_return() {
    check(
        "shebang",
        "#!/usr/bin/env ruby\r\np 1\r\n",
        &[(1, SHEBANG)],
        &[(1, SHEBANG)],
    );
    check("shebang_lf", "#!/usr/bin/env ruby\np 1\n", &[], &[]);
    // The `#!ruby` line a script that starts as something else is
    // searched for (what `-x` does).
    let source = "#!/bin/sh\nexec ruby -x \"$0\"\n#!ruby\r\np 1\n";
    check("shebang_x", source, &[(3, SHEBANG)], &[(3, SHEBANG)]);
    let script = Script::new("shebang_dash_x", source);
    assert_eq!(
        monoruby_stderr(&["-x", script.path()])
            .lines()
            .collect::<Vec<_>>(),
        script.lines(&[(3, SHEBANG)])
    );
    // A script on stdin and a `-e` script are main scripts too …
    let mut cmd = monoruby(&["-w"]);
    cmd.stdin(std::fs::File::open(script.path()).unwrap());
    assert_eq!(stderr(cmd), format!("-:3: warning: {SHEBANG}\n"));
    assert_eq!(
        monoruby_stderr(&["-e", "#!ruby\r\np 1"]),
        format!("-e:1: warning: {SHEBANG}\n")
    );
    // … and a file that is loaded is not.
    let loaded = Script::new("shebang_loaded", "#!ruby\r\n1.itself\r\n");
    let load = format!("load {:?}; require {:?}", loaded.path(), loaded.path());
    assert_eq!(monoruby_stderr(&["-w", "-e", &load]), "");
}

#[test]
fn in_a_dash_e_script_an_eval_and_a_loaded_file() {
    let binary = format!("warning: '+' {BINARY} unary operator");
    // `-e` keeps a regexp condition quiet (the awk idiom) and nothing
    // else.
    let err = monoruby_stderr(&[
        "-w",
        "-e",
        "a = 1; p(a +1); eval(\"b = 1; p(b +1); if 1 then end; 2\")",
        "-e",
        "p 1 if 1",
        "-e",
        "p 2 if /a/",
        "-e",
        "p 3 if \"s\"",
    ]);
    let expected = [
        format!("-e:1: {binary}"),
        "-e:2: warning: literal in condition".to_string(),
        "-e:4: warning: string literal in condition".to_string(),
        format!("(eval at -e:1):1: {binary}"),
        "(eval at -e:1):1: warning: literal in condition".to_string(),
    ];
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
    // A loaded file's warnings go by `$VERBOSE` at the time it loads.
    let loaded = Script::new("loaded", "a = 1\np(a +1)\nif \"s\" then end\n");
    let file = loaded.path();
    let load =
        format!("load {file:?}; $VERBOSE = true; load {file:?}; $VERBOSE = nil; load {file:?}");
    let err = monoruby_stderr(&["-e", &load]);
    let expected = [
        format!("{file}:3: warning: string literal in condition"),
        format!("{file}:2: {binary}"),
        format!("{file}:3: warning: string literal in condition"),
    ];
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
}

#[test]
fn a_main_script_compiled_twice_warns_once() {
    // A main script that names TOPLEVEL_BINDING is compiled as a plain
    // top level, abandoned, and compiled again inside the binding. The
    // warnings are the first parse's — with the `#!` line's, and under
    // `-e` without the regexp condition's.
    let binary = format!("'+' {BINARY} unary operator");
    let script = Script::new(
        "toplevel_binding",
        "#!ruby\r\nb = TOPLEVEL_BINDING\r\na = 1\r\np(a +1)\r\nif \"s\" then end\r\n1\r\n\
         p b.local_variables\r\n",
    );
    let out = monoruby(&["-w", script.path()]).output().unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stdout), "2\n[:b, :a]\n");
    let err = String::from_utf8_lossy(&out.stderr).into_owned();
    let expected = script.lines(&[
        (1, SHEBANG),
        (4, &binary),
        (5, "string literal in condition"),
        (6, "possibly useless use of a literal in void context"),
    ]);
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
    assert_eq!(
        monoruby_stderr(&["-e", "TOPLEVEL_BINDING; p 1 if /a/; p 2 if \"s\""]),
        "-e:1: warning: string literal in condition\n"
    );
}

#[test]
fn every_dash_e_is_a_line() {
    // CRuby ends each `-e` with a newline, so a `\r` closing one (a
    // CRLF shell script) is a line ending and not "\r in middle of
    // line".
    let out = monoruby(&["-w", "-e", "p 1\r", "-e", "p __LINE__\r"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "1\n2\n");
    // In the middle of one it is not.
    assert_eq!(
        monoruby_stderr(&["-e", "a = 1 \r + 2"]),
        "-e:1: warning: encountered \\r in middle of line, treated as a mere space\n"
    );
}
