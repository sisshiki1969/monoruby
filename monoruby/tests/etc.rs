//! `Etc` is CRuby's `ext/etc` built on the C API layer
//! (`monoruby_capi/ext/etc`). These pin the whole module against the host
//! CRuby on the machine the test runs on: the user and group databases,
//! the kernel's answers and the locale are the environment's, so every
//! comparison is against a live CRuby, never the snapshot. Each answer is
//! compared as its `inspect` (the harness re-reads CRuby's output as Ruby
//! source, which a Struct's or an Encoding's is not).

extern crate monoruby;
use monoruby::tests::*;

/// What the extension defines: the feature is a `.so`, the module
/// functions, the `SC_` / `CS_` / `PC_` constants `constdefs.h` finds in
/// the platform's headers, and the two Structs with `Enumerable` on their
/// singleton classes.
#[test]
fn etc_module_surface() {
    run_test_once_live(
        r#"
        require "etc"
        [
          $LOAD_PATH.resolve_feature_path("etc")&.first,
          require("etc"),
          Etc::VERSION,
          Etc.methods(false).sort,
          Etc.private_instance_methods(false).sort,
          Etc.constants.sort.map { |c| v = Etc.const_get(c); [c, v.is_a?(Integer) ? v : v.class] },
          Etc::Passwd.ancestors.take(3), Etc::Passwd.members, Etc::Passwd.name,
          Etc::Passwd.singleton_class.include?(Enumerable),
          Etc::Group.ancestors.take(3), Etc::Group.members,
          Etc::Group.singleton_class.include?(Enumerable),
          Etc.method(:getpwuid).arity, Etc.method(:getpwnam).arity, Etc.method(:uname).owner,
          IO.instance_method(:pathconf).owner,
          Etc::Passwd.new("a", "b", 1, 2).to_a,
          (Struct::Passwd rescue $!.class), (Struct::Group rescue $!.class),
        ].map(&:inspect)
        "#,
    );
}

/// Lookups by id and by name, the fields' classes and encodings (names
/// in the locale encoding, the password field BINARY), and the errors of
/// `NUM2UIDT` / `StringValueCStr`.
#[test]
fn etc_passwd_and_group_lookup() {
    run_test_once_live(
        r#"
        require "etc"
        t = ->(&b) { b.call.inspect rescue [$!.class, $!.message].inspect }
        shape = ->(s) { s.to_a.map { |x| [x.class, (x.encoding if x.is_a?(String)), x.frozen?] } }
        gr = Etc.getgrgid(0)
        [
          t.() { Etc.getpwuid },
          t.() { Etc.getpwuid(0) },
          t.() { Etc.getpwuid(Process.uid).uid == Process.uid },
          t.() { shape.(Etc.getpwuid(0)) },
          t.() { Etc.getpwnam("root") },
          t.() { Etc.getpwnam("root").to_h },
          t.() { Etc.getpwnam("daemon").inspect },
          t.() { Etc.getpwuid(0) == Etc.getpwnam("root") },
          t.() { pw = Etc.getpwuid(0); pw.name = "x"; [pw.name, Etc.getpwuid(0).name] },
          t.() { Marshal.load(Marshal.dump(Etc.getpwuid(0))) == Etc.getpwuid(0) },
          t.() { Etc.getpwnam("no_such_user_zz") },
          t.() { Etc.getpwuid(123456789) },
          t.() { Etc.getpwuid("a") },
          t.() { Etc.getpwuid(nil) },
          t.() { Etc.getpwuid(1, 2) },
          t.() { Etc.getpwuid(2**40) },
          t.() { Etc.getpwnam(1) },
          t.() { Etc.getpwnam(:root) },
          t.() { Etc.getpwnam("a\0b") },
          t.() { Etc.getgrgid },
          t.() { gr },
          t.() { shape.(gr) },
          t.() { Etc.getgrnam(gr.name) },
          t.() { Etc.getgrgid(Process.gid).gid == Process.gid },
          t.() { Etc.getgrnam("no_such_group_zz") },
          t.() { Etc.getgrgid(123456789) },
          t.() { Etc.getgrgid("a") },
          t.() { Etc.getgrgid(nil) },
          t.() { Etc.getgrnam(1) },
          t.() { Etc.getgrnam("a\0b") },
        ]
        "#,
    );
}

/// Walking the databases: `Etc.passwd` / `Etc.group` with and without a
/// block, the `set` / `get` / `end` triple, `Enumerable` over the Structs,
/// leaving the walk by `break` and by an exception (`rb_ensure` closes
/// it), and the guard against a nested walk.
#[test]
fn etc_database_enumeration() {
    run_test_once_live(
        r#"
        require "etc"
        t = ->(&b) { b.call.inspect rescue [$!.class, $!.message].inspect }
        [
          t.() { a = []; Etc.passwd { |pw| a << [pw.name, pw.uid] }; a },
          t.() { Etc.endpwent; Etc.passwd.class },
          t.() { Etc.setpwent; a = []; while (pw = Etc.getpwent); a << pw.uid; end; Etc.endpwent; a },
          t.() { Etc.setpwent; x = Etc.getpwent.name; Etc.setpwent; [x, Etc.getpwent.name, Etc.endpwent] },
          t.() { Etc::Passwd.each.class },
          t.() { Etc::Passwd.map(&:name) },
          t.() { Etc::Passwd.count },
          t.() { Etc::Passwd.find { |pw| pw.uid == 1 }&.name },
          t.() { Etc::Passwd.each { |pw| break pw.name } },
          t.() { Etc.passwd { Etc.passwd { } } },
          t.() { Etc.passwd { Etc::Passwd.each { } } },
          t.() { Etc.passwd { raise "x" } },
          t.() { Etc.passwd { |pw| break pw.name } },
          t.() { Etc.getpwent.name },
          t.() { Etc.endpwent },
          t.() { a = []; Etc.group { |gr| a << [gr.name, gr.gid, gr.mem] }; a },
          t.() { Etc.endgrent; Etc.group.class },
          t.() { Etc.setgrent; a = []; while (gr = Etc.getgrent); a << gr.gid; end; Etc.endgrent; a },
          t.() { Etc::Group.map(&:name) },
          t.() { Etc::Group.each { |g| break g.gid } },
          t.() { Etc.group { Etc.group { } } },
          t.() { Etc.group { raise "x" } },
          t.() { Etc.getgrent.name },
          t.() { Etc.endgrent },
        ]
        "#,
    );
}

/// The kernel's answers: the processor count the affinity mask allows,
/// `uname`, `sysconf` / `confstr` / `IO#pathconf` with their `nil` for
/// "no limit" and their Errno for an unknown name, and the directories.
/// (`Etc.sysconfdir` is the configured prefix's, so only its shape is
/// compared.)
#[test]
fn etc_system_information() {
    run_test_once_live(
        r#"
        require "etc"
        t = ->(&b) { b.call.inspect rescue [$!.class, $!.message].inspect }
        r, w = IO.pipe
        res = [
          t.() { Etc.nprocessors },
          t.() { Etc.uname },
          t.() { Etc.uname.map { |k, v| [k.class, v.class, v.encoding, v.frozen?] }.uniq },
          t.() { Etc.getlogin },
          t.() { Etc.sysconf(Etc::SC_NPROCESSORS_ONLN) },
          t.() { Etc.sysconf(Etc::SC_ARG_MAX) },
          t.() { Etc.sysconf(Etc::SC_PAGESIZE) },
          t.() { Etc.sysconf(Etc::SC_2_FORT_DEV) },
          t.() { Etc.sysconf(-1) },
          t.() { Etc.sysconf("x") },
          t.() { Etc.sysconf(nil) },
          t.() { Etc.confstr(Etc::CS_PATH) },
          t.() { s = Etc.confstr(Etc::CS_PATH); [s.encoding, s.frozen?] },
          t.() { Etc.confstr(-1) },
          t.() { r.pathconf(Etc::PC_PIPE_BUF) },
          t.() { File.open("/") { |f| f.pathconf(Etc::PC_NAME_MAX) } },
          t.() { File.open("/") { |f| f.pathconf(Etc::PC_SYNC_IO) } },
          t.() { File.open("/") { |f| f.pathconf(-1) } },
          t.() { s = Etc.systmpdir; [s, s.encoding, s.frozen?] },
          t.() { s = Etc.sysconfdir; [s.class, s.encoding, s.frozen?] },
          t.() { Object.new.extend(Etc).send(:nprocessors) },
        ]
        r.close; w.close
        res
        "#,
    );
}
