//! What a *concurrent* observer sees of an autoload that is in
//! progress (#1426).
//!
//! CRuby's rule is that an in-flight autoload is visible per-thread:
//! the thread running the load sees the constant it has assigned (and
//! sees the slot as undefined until it does), while every other thread
//! goes on seeing the registered autoload it saw before the load
//! started — and blocks, rather than raising or starting a second load,
//! when it actually reads the value.
//!
//! Two single-threaded rules about *several constants sharing one
//! autoload file* are here too, because the concurrent case rests on
//! them: which constants a load retires depends on whether the load was
//! a direct `require` or an autoload's own, and a retired one still
//! re-runs its file once the feature is dropped from
//! `$LOADED_FEATURES`. Getting either wrong stays invisible until many
//! threads race on one file, which is how they were found.
//!
//! These spawn the real binary: green threads and a file `require` are
//! both process-level, so an in-process `run_test` cannot reach them.
//! The expectations are differential — each case asks the reference
//! CRuby what it answers and requires the same answer — because the
//! fixture paths are temporary and the interleaving is the property
//! under test, not any particular literal.

use std::process::Command;

fn apply(cmd: &mut Command) {
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .env_remove("RUBYPATH");
}

fn monoruby() -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    apply(&mut cmd);
    cmd.arg("--disable=gems");
    cmd
}

fn cruby() -> Command {
    let mut cmd = Command::new(monoruby::tests::ruby_path());
    apply(&mut cmd);
    cmd.arg("--disable=gems,rubyopt");
    cmd
}

fn run_script(mut cmd: Command, dir: &std::path::Path, path: &std::path::Path) -> String {
    let out = cmd
        .arg(path)
        .current_dir(dir)
        .output()
        .expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} exited with {:?}\nstderr: {}",
        cmd,
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// A private directory for one test's fixtures. Each test needs its own
/// because the probe autoloads a fresh file per case and the file names
/// end up in the compared output.
fn fixture_dir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("mr_autoload_{name}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create fixture dir");
    dir
}

fn write(dir: &std::path::Path, name: &str, body: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, body).expect("write script");
    path
}

fn assert_same(label: &str, dir: &std::path::Path, script: &std::path::Path) {
    let expected = run_script(cruby(), dir, script);
    let actual = run_script(monoruby(), dir, script);
    if expected != actual {
        let first_diff = expected
            .lines()
            .zip(actual.lines())
            .find(|(e, a)| e != a)
            .map(|(e, a)| format!("cruby {e:?} vs monoruby {a:?}"))
            .unwrap_or_else(|| "line counts differ".to_string());
        panic!(
            "{label} differs\nfirst difference: {first_diff}\n\
             expected (cruby):\n{expected}\nactual (monoruby):\n{actual}"
        );
    }
}

/// ruby/spec's `check_before_during_thread_after`: run `check` four
/// times — before the autoload, inside the loading thread (from the
/// autoloaded file itself), in another thread while the loading thread
/// is parked mid-load, and after the load finished.
const HARNESS: &str = r##"
module AL; end
DIR = File.expand_path(File.dirname(__FILE__))

def check_before_during_thread_after(const, &check)
  before = check.call
  to_t, from_t = Queue.new, Queue.new
  $SP = -> { from_t.push check.call; to_t.pop }
  t = Thread.new {
    in_loading = from_t.pop
    in_other = check.call
    to_t.push :done
    [in_loading, in_other]
  }
  begin
    AL.const_get(const)
  ensure
    in_loading, in_other = t.value
  end
  [before, in_loading, in_other, check.call]
end

$n = 0
# `assigned` picks where the fixture runs the probe block: before it
# assigns the constant, or from inside the class body after.
def fixture(const, assigned)
  $n += 1
  path = File.join(DIR, "f#{$n}.rb")
  body = if assigned
    "module AL\n  class #{const}\n    block = $SP\n    $SP = block.call\n  end\nend\n"
  else
    "block = $SP\n$SP = block.call\n\nmodule AL\n  class #{const}\n  end\nend\n"
  end
  File.write(path, body)
  AL.autoload const.to_sym, path
  path
end

def show(label, r)
  puts "#{label}: #{r.map { |x| x.inspect.gsub(DIR + "/", "") }.join(' | ')}"
end
"##;

/// The four `[before, loading thread, other thread, after]` slots for
/// every reader that can see an autoload, in both the "the file has not
/// assigned the constant yet" and "it already has" cases.
#[test]
fn an_in_flight_autoload_is_visible_per_thread() {
    let dir = fixture_dir("visibility");
    let script = write(
        &dir,
        "probe.rb",
        &format!(
            r#"{HARNESS}
c = :B1; fixture(c, false); show "defined?        ", (check_before_during_thread_after(c) {{ defined?(AL::B1) }})
c = :B2; fixture(c, false); show "constants       ", (check_before_during_thread_after(c) {{ AL.constants(false).include?(:B2) }})
c = :B3; fixture(c, false); show "const_defined?  ", (check_before_during_thread_after(c) {{ AL.const_defined?(:B3, false) }})
c = :B4; fixture(c, false); show "autoload?       ", (check_before_during_thread_after(c) {{ AL.autoload?(:B4) }})
c = :A1; fixture(c, true);  show "A defined?      ", (check_before_during_thread_after(c) {{ defined?(AL::A1) }})
c = :A2; fixture(c, true);  show "A constants     ", (check_before_during_thread_after(c) {{ AL.constants(false).include?(:A2) }})
c = :A3; fixture(c, true);  show "A const_defined?", (check_before_during_thread_after(c) {{ AL.const_defined?(:A3, false) }})
c = :A4; fixture(c, true);  show "A autoload?     ", (check_before_during_thread_after(c) {{ AL.autoload?(:A4) }})
c = :A5; fixture(c, true);  show "A const_src_loc ", (check_before_during_thread_after(c) {{ AL.const_source_location(:A5) }})
"#
        ),
    );
    assert_same("per-thread autoload visibility", &dir, &script);
}

/// A second thread reading a constant whose autoload is already running
/// waits for it and gets the value, rather than raising `NameError` or
/// running the file a second time.
#[test]
fn a_second_thread_waits_for_the_first_thread_s_autoload() {
    let dir = fixture_dir("blocks");
    write(
        &dir,
        "concur.rb",
        r#"
$order << :file_start
Thread.current[:in_file] = true
sleep 0.1
module AL
  Concur = 1
end
$order << :file_end
"#,
    );
    let script = write(
        &dir,
        "probe.rb",
        r#"
module AL; end
$order = []
AL.autoload :Concur, File.expand_path("concur.rb", File.dirname(__FILE__))

start = false
t1_val = t2_val = t2_exc = nil
fin = false

t1 = Thread.new do
  Thread.pass until start
  t1_val = AL::Concur
  $order << :t1_read
  fin = true
end
t2 = Thread.new do
  Thread.pass until t1[:in_file]
  begin
    t2_val = AL::Concur
  rescue Exception => e
    t2_exc = e.class
  else
    Thread.pass until fin
    $order << :t2_read
  end
end
start = true
t1.join
t2.join

p $order
p t1_val, t2_val, t2_exc
"#,
    );
    assert_same("a second thread waits for the loader", &dir, &script);
}

/// Several constants registered to the *same* file, hammered by many
/// threads at once, with the feature dropped from `$LOADED_FEATURES`
/// between rounds so the file runs again: every thread must come away
/// with a fully built module, and the file must be evaluated exactly
/// once per round. ruby/spec's "blocks others threads while doing an
/// autoload" (https://bugs.ruby-lang.org/issues/10892), shrunk to keep
/// the runtime down.
#[test]
fn many_threads_racing_on_one_autoload_file_evaluate_it_once_per_round() {
    let dir = fixture_dir("repeated");
    write(
        &dir,
        "repeated.rb",
        r#"
prev = $CTR.increment_and_get
eval <<-RUBY_EVAL
  module Mod#{prev}
    sleep(0.02)
    def self.foo
    end
  end
RUBY_EVAL
"#,
    );
    let script = write(
        &dir,
        "probe.rb",
        r#"
class ThreadSafeCounter
  def initialize; @value = 0; @mutex = Mutex.new; end
  def get; @mutex.synchronize { @value }; end
  def increment_and_get
    @mutex.synchronize { prev = @value; @value += 1; prev }
  end
end

class CyclicBarrier
  def initialize(count) = (@count, @state, @mutex, @cond = count, 0, Mutex.new, ConditionVariable.new)
  def await
    @mutex.synchronize do
      @state += 1
      if @state >= @count then @state = 0; @cond.broadcast; true
      else @cond.wait @mutex; false end
    end
  end
  def enabled? = @mutex.synchronize { @count != -1 }
  def disable! = @mutex.synchronize { @count = -1; @cond.broadcast }
end

file_path     = File.expand_path("repeated.rb", File.dirname(__FILE__))
autoload_path = file_path.sub(/\.rb\Z/, '')
mod_count     = 6
thread_count  = 4

mod_names = mod_count.times.map { |i| n = :"Mod#{i}"; Object.autoload n, autoload_path; n }

barrier = CyclicBarrier.new thread_count
$CTR = ThreadSafeCounter.new

threads = (1..thread_count).map do
  Thread.new do
    mod_names.each do |mod_name|
      break false unless barrier.enabled?
      last_in = barrier.await
      $LOADED_FEATURES.delete(file_path) if last_in && $LOADED_FEATURES.include?(file_path)
      barrier.await
      begin
        Object.const_get(mod_name).foo
      rescue NameError, NoMethodError
        barrier.disable!
        break false
      end
    end
  end
end

p threads.all? { |t| t.value }
p $CTR.get
"#,
    );
    assert_same("repeated concurrent autoload", &dir, &script);
}

/// A *direct* `require` of a file retires every constant registered to
/// autoload it: the predicates stop reporting them, though the names
/// stay in `Module#constants`. Reading one still runs `require` — a
/// no-op while the feature is loaded, but once it is dropped from
/// `$LOADED_FEATURES` the file runs again and may define the constant
/// after all.
#[test]
fn a_direct_require_retires_the_other_constants_but_still_retries_the_file() {
    let dir = fixture_dir("direct");
    write(
        &dir,
        "shared.rb",
        "$loads = ($loads || 0) + 1\nObject.const_set(:D1, $loads)\n",
    );
    let script = write(
        &dir,
        "probe.rb",
        r##"
DIR = File.expand_path(File.dirname(__FILE__))
P = File.join(DIR, "shared.rb")
def show(label, v) = puts("#{label}: #{v.inspect.gsub(DIR + "/", "")}")

# The file defines D1 and nothing else, so D2's registration is the one
# the direct require retires.
Object.autoload :D1, P
Object.autoload :D2, P
require P
show "loads         ", $loads
show "autoload? D1  ", Object.autoload?(:D1)
show "autoload? D2  ", Object.autoload?(:D2)
show "defined? D2   ", Object.const_defined?(:D2)
show "constants D2  ", Object.constants.include?(:D2)

begin; Object::D2; rescue NameError => e; show "read D2       ", e.class; end
show "loads         ", $loads

# Dropped from $LOADED_FEATURES, the retired registration runs the file
# again — it still does not define D2, and the slot stays retired.
$LOADED_FEATURES.delete(P)
begin; Object::D2; rescue NameError => e; show "read D2 again ", e.class; end
show "loads         ", $loads
show "autoload? D2  ", Object.autoload?(:D2)
show "constants D2  ", Object.constants.include?(:D2)
"##,
    );
    assert_same("direct require of a shared file", &dir, &script);
}

/// An *autoload's own* require leaves the other constants registered to
/// the same file completely alone — `autoload?` keeps answering the
/// path for them. Retiring them here is what made ruby/spec's
/// repeated-autoload case fail from its second round on.
#[test]
fn an_autoload_s_own_require_leaves_the_other_constants_registered() {
    let dir = fixture_dir("triggered");
    write(
        &dir,
        "shared.rb",
        "$loads = ($loads || 0) + 1\nObject.const_set(:E1, $loads)\n",
    );
    let script = write(
        &dir,
        "probe.rb",
        r##"
DIR = File.expand_path(File.dirname(__FILE__))
P = File.join(DIR, "shared.rb")
def show(label, v) = puts("#{label}: #{v.inspect.gsub(DIR + "/", "")}")

Object.autoload :E1, P
Object.autoload :E2, P
show "E1            ", Object::E1
show "loads         ", $loads
show "autoload? E2  ", Object.autoload?(:E2)
show "defined? E2   ", Object.const_defined?(:E2)
show "constants E2  ", Object.constants.include?(:E2)

# E2's own registration is live, so reading it loads the file — already
# loaded, so a no-op — and then reports the constant missing for good.
begin; Object::E2; rescue NameError => e; show "read E2       ", e.class; end
show "loads         ", $loads
show "autoload? E2  ", Object.autoload?(:E2)
"##,
    );
    assert_same("autoload-triggered require of a shared file", &dir, &script);
}
