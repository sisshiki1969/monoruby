//! prism's own parse warnings (#2126), each printed as CRuby 4.0.9
//! prints it and at the level CRuby prints it at.
//!
//! prism reports thirty kinds of warning (`PM_WARN_*`); monoruby used
//! to forward nine of them. The expectations below are CRuby's output
//! for the same script under `-w` (every warning) and without it (the
//! default-level ones only).
//!
//! Also: the order of a scope's "assigned but unused variable" warnings
//! (#2192), and the warnings of a source that ends in a SyntaxError
//! (#2191).

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

/// `(line, name)` pairs as "assigned but unused variable" warnings.
fn unused(locals: &[(u32, &str)]) -> Vec<(u32, String)> {
    locals
        .iter()
        .map(|(line, name)| (*line, format!("assigned but unused variable - {name}")))
        .collect()
}

fn check_unused(name: &str, source: &str, locals: &[(u32, &str)]) {
    let expected = unused(locals);
    let expected: Vec<(u32, &str)> = expected.iter().map(|(l, m)| (*l, m.as_str())).collect();
    check(name, source, &expected, &[]);
}

// #2192. prism reports the unused locals of a scope by walking the
// scope's table of locals, and the table of the prism linked here (2.x)
// is not the one of CRuby 4.0's (1.8): it becomes a hash table at three
// locals where CRuby's stays a list, in the order of the first writes,
// up to six.
#[test]
fn unused_locals_of_a_small_scope_in_source_order() {
    // The script of the issue. prism 2.x alone says b, c, a twice.
    check_unused(
        "unused_small",
        "def m\n  a = 1\n  b = 2\n  c = 3\nend\ndef n\n  c = 1; b = 2; a = 3\nend\n",
        &[(2, "a"), (3, "b"), (4, "c"), (7, "c"), (7, "b"), (7, "a")],
    );
}

#[test]
fn unused_locals_where_the_table_starts_hashing() {
    // Six locals fill CRuby's list. The write that finds it full turns
    // it into a hash table, and only a write that adds a local with a
    // plain `name = value` is followed by another one (prism writes the
    // name before the value and again after it):
    // - `m`: the sixth local is added by `a = 1`, so the table hashes;
    // - `n`: it is added as a target of `b, a = 1, 2`, and stays a list;
    // - `o`: two of the six are parameters, `f = e` adds nothing;
    // - `q`: the targets are added inside the value of `g = (...)`,
    //   whose second write follows them.
    check_unused(
        "unused_six",
        "def m\n  f = 1\n  e = 1\n  d = 1\n  c = 1\n  b = 1\n  a = 1\nend\n\
         def n\n  f = 1\n  e = 1\n  d = 1\n  c = 1\n  b, a = 1, 2\nend\n\
         def o(f, e)\n  d = 1\n  c = 1\n  b = 1\n  a = 1\n  f = e\nend\n\
         def q\n  f = 1\n  e = 1\n  d = 1\n  c = 1\n  g = (b, a = 1, 2)\nend\n",
        &[
            (5, "c"),
            (6, "b"),
            (4, "d"),
            (2, "f"),
            (3, "e"),
            (7, "a"),
            (10, "f"),
            (11, "e"),
            (12, "d"),
            (13, "c"),
            (14, "b"),
            (14, "a"),
            (18, "c"),
            (19, "b"),
            (17, "d"),
            (20, "a"),
            (27, "c"),
            (28, "b"),
            (28, "g"),
            (26, "d"),
            (24, "f"),
            (25, "e"),
            (28, "a"),
        ],
    );
}

#[test]
fn unused_locals_where_the_hash_table_grows() {
    // Twelve locals: the table doubles to 32 slots in `m`, whose last
    // local is followed by a write, and keeps its 16 in `n`.
    check_unused(
        "unused_twelve",
        "def m\n  l = 1\n  k = 1\n  j = 1\n  i = 1\n  h = 1\n  g = 1\n  f = 1\n  e = 1\n  \
         d = 1\n  c = 1\n  b = 1\n  a = 1\nend\n\
         def n\n  l = 1\n  k = 1\n  j = 1\n  i = 1\n  h = 1\n  g = 1\n  f = 1\n  e = 1\n  \
         d = 1\n  c = 1\n  b, a = 1, 2\nend\n",
        &[
            (9, "e"),
            (11, "c"),
            (4, "j"),
            (2, "l"),
            (7, "g"),
            (10, "d"),
            (12, "b"),
            (13, "a"),
            (5, "i"),
            (6, "h"),
            (8, "f"),
            (3, "k"),
            (19, "i"),
            (20, "h"),
            (22, "f"),
            (23, "e"),
            (18, "j"),
            (25, "c"),
            (16, "l"),
            (17, "k"),
            (21, "g"),
            (24, "d"),
            (26, "b"),
            (26, "a"),
        ],
    );
}

#[test]
fn unused_locals_of_every_kind_of_scope() {
    // A block, a lambda, a class, a module, a singleton class, a method
    // with parameters and the top level, each with its own table. What a
    // nested scope leaves to the enclosing one — a superclass, the path
    // of a module, the object of `class <<`, a default value — writes
    // the enclosing scope's locals (`kd`, `ke`, `kf`, which make `K`'s
    // table a hash table; `rb`, `ra`).
    check_unused(
        "unused_scopes",
        "def o\n  z = 1\n  [1].each do |x|\n    y = 2\n    w = 3\n    -> { q = 1; p0 = 2 }\n  \
         end\n  v = 2\nend\n\
         class K\n  kb = 1\n  ka = 2\n  def km; kz = 1; ky = 2; end\n  def self.ks; end\n  \
         class Inner < (kd = Object); end\n  module (ke = self)::Mod; end\n  \
         class << (kf = self); end\n  [1].each { |i| }\n  -> { }\n  kc = 3\nend\n\
         module M\n  mb = 1; ma = 2\nend\n\
         class << self\n  sb = 1; sa = 2\nend\n\
         def r(pb = (rb = 1), *rest, pk: (ra = 2), **opts, &blk)\n  rd = 1; rc = 2\n  \
         pb = rest = pk = opts = blk\nend\n\
         [1].each do |i; lb, la|\n  lb = 1; la = 2\n  bd = 1; bc = 2; bb = 3; i = bb\n  \
         bb += 1\n  ba &&= 1\n  b9 ||= 1\nend\n\
         tb = 1\nta = 2\n",
        &[
            (6, "q"),
            (6, "p0"),
            (4, "y"),
            (5, "w"),
            (2, "z"),
            (8, "v"),
            (13, "kz"),
            (13, "ky"),
            (20, "kc"),
            (11, "kb"),
            (12, "ka"),
            (23, "mb"),
            (23, "ma"),
            (26, "sb"),
            (26, "sa"),
            (29, "rd"),
            (29, "rc"),
            (34, "bc"),
            (34, "bd"),
            (39, "tb"),
            (40, "ta"),
        ],
    );
}

/// stderr of a run that is expected to fail, with its exit status.
fn failing_stderr(args: &[&str]) -> String {
    let out = monoruby(args).output().expect("failed to spawn monoruby");
    assert!(!out.status.success(), "{out:?}");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// A source with a warning of each level, an unused local and, at the
/// end, a syntax error.
const BROKEN: &str = "a = 1; p(a +1); if 'x' then end; b = 2; )";

// #2191. CRuby prints the warnings of a source before it raises the
// source's SyntaxError; monoruby returned the error first and never
// printed them.
#[test]
fn a_script_that_does_not_parse_has_its_warnings() {
    let script = Script::new("broken_main", &format!("{BROKEN}\n"));
    let file = script.path();
    let binary = format!("'+' {BINARY} unary operator");
    let verbose = script.lines(&[
        (1, &binary),
        (1, "string literal in condition"),
        (1, "assigned but unused variable - b"),
    ]);
    let default = script.lines(&[(1, "string literal in condition")]);
    for (flags, expected) in [
        (&["-w"][..], &verbose),
        (&[][..], &default),
        (&["-W1"][..], &default),
        (&["-W0"][..], &vec![]),
        // Under `-c` too, where nothing but the parse happens.
        (&["-wc"][..], &verbose),
        (&["-c"][..], &default),
        (&["-W0", "-c"][..], &vec![]),
    ] {
        let err = failing_stderr(&[flags, &[file]].concat());
        // The warnings come first; the error follows.
        let lines: Vec<&str> = err.lines().collect();
        assert!(lines.len() > expected.len(), "{flags:?}: {err}");
        assert_eq!(&lines[..expected.len()], &expected[..], "{flags:?}: {err}");
        assert!(
            !lines[expected.len()..]
                .iter()
                .any(|l| l.contains(": warning: ")),
            "{flags:?}: {err}"
        );
        assert!(err.contains("SyntaxError"), "{flags:?}: {err}");
    }
}

#[test]
fn an_eval_or_a_loaded_file_that_does_not_parse_has_its_warnings() {
    let binary = format!("'+' {BINARY} unary operator");
    let lib = Script::new("broken_lib", &format!("{BROKEN}\n"));
    // Each failing parse is announced on stderr by the line that runs
    // it, so that the expectation also says *when* its warnings are
    // printed: before the SyntaxError reaches the `rescue`.
    let main = Script::new(
        "broken_evals",
        &format!(
            "SRC = \"{BROKEN}\"\n\
             def t(label)\n  $stderr.puts label\n  yield\n  $stderr.puts \"no error\"\n\
             rescue SyntaxError\n  $stderr.puts \"rescued\"\nend\n\
             t(\"eval\") {{ eval(SRC) }}\n\
             t(\"eval with a binding\") {{ eval(SRC, binding) }}\n\
             t(\"eval with a file\") {{ eval(SRC, nil, \"foo.rb\", 10) }}\n\
             t(\"Binding#eval\") {{ binding.eval(SRC) }}\n\
             t(\"instance_eval\") {{ Object.new.instance_eval(SRC) }}\n\
             t(\"class_eval\") {{ Class.new.class_eval(SRC) }}\n\
             t(\"module_eval with a file\") {{ Module.new.module_eval(SRC, \"bar.rb\", 5) }}\n\
             t(\"require\") {{ require {lib:?} }}\n\
             t(\"load\") {{ load {lib:?} }}\n\
             t(\"load in a module\") {{ load {lib:?}, true }}\n\
             t(\"a jump with nothing to jump out of\") {{ eval(\"if 'y' then end; retry\") }}\n\
             t(\"an eval that parses\") {{ eval(\"if 'z' then end\") }}\n",
            lib = lib.path(),
        ),
    );
    let file = main.path();
    // (announcement, the name the warnings carry, whether the source
    // is a file: its top level reports its unused locals).
    let sources = [
        ("eval", format!("(eval at {file}:9)"), 1, false),
        (
            "eval with a binding",
            format!("(eval at {file}:10)"),
            1,
            false,
        ),
        ("eval with a file", "foo.rb".to_string(), 10, false),
        ("Binding#eval", format!("(eval at {file}:12)"), 1, false),
        ("instance_eval", format!("(eval at {file}:13)"), 1, false),
        ("class_eval", format!("(eval at {file}:14)"), 1, false),
        ("module_eval with a file", "bar.rb".to_string(), 5, false),
        ("require", lib.path().to_string(), 1, true),
        ("load", lib.path().to_string(), 1, true),
        ("load in a module", lib.path().to_string(), 1, true),
    ];
    let expected = |verbose: bool, warnings: bool| {
        let mut lines = vec![];
        for (label, name, line, is_file) in &sources {
            lines.push(label.to_string());
            if warnings && verbose {
                lines.push(format!("{name}:{line}: warning: {binary}"));
            }
            if warnings {
                lines.push(format!(
                    "{name}:{line}: warning: string literal in condition"
                ));
            }
            if warnings && verbose && *is_file {
                lines.push(format!(
                    "{name}:{line}: warning: assigned but unused variable - b"
                ));
            }
            lines.push("rescued".to_string());
        }
        lines.push("a jump with nothing to jump out of".to_string());
        if warnings {
            lines.push(format!(
                "(eval at {file}:19):1: warning: string literal in condition"
            ));
        }
        lines.push("rescued".to_string());
        lines.push("an eval that parses".to_string());
        if warnings {
            lines.push(format!(
                "(eval at {file}:20):1: warning: string literal in condition"
            ));
        }
        lines.push("no error".to_string());
        lines
    };
    for (flags, verbose, warnings) in [
        (&["-w"][..], true, true),
        (&[][..], false, true),
        (&["-W0"][..], false, false),
    ] {
        let err = monoruby_stderr(&[flags, &[file]].concat());
        assert_eq!(
            err.lines().collect::<Vec<_>>(),
            expected(verbose, warnings),
            "{flags:?}: {err}"
        );
    }
}
