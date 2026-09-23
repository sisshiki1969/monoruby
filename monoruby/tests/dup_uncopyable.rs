extern crate monoruby;
use monoruby::tests::*;

// `dup` / `clone` of the objects whose state is an execution context or
// that CRuby has no allocator for (#1624). An Enumerator, Generator or
// Fiber used to reach an `unreachable!` arm in `RValue::dup` and abort
// the process; a Thread, a Ractor and the thread-sync queues were copied
// where CRuby refuses.

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message.gsub(/0x\h+/, "0x")]; end }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// A copy of an Enumerator (and of Lazy / Chain / Product) is a fresh
/// enumerator over the same source; one whose external iteration is part
/// way through is `TypeError: can't copy execution context`, and an
/// uninitialized one is `ArgumentError`.
#[test]
fn an_enumerator_copies_its_source_not_its_position() {
    run(r#"
        res = []
        %i[dup clone].each do |m|
          res << t.() { x = [1, 2].each; y = x.__send__(m); [y.class.to_s, y.to_a, y.next, x.next, y.equal?(x)] }
          res << t.() { [1, 2, 3].each_slice(2).__send__(m).to_a }
          res << t.() { x = [1, 2, 3].each; x.next; x.__send__(m) }
          res << t.() { x = [1].each; x.peek; x.__send__(m) }
          res << t.() { x = Enumerator.new { |yy| yy << 1; yy << 2 }; y = x.__send__(m); [y.to_a, y.next] }
          res << t.() { Enumerator.new(3) { |yy| yy << 1 }.__send__(m).size }
          res << t.() { (1..Float::INFINITY).lazy.map { _1 * 2 }.__send__(m).first(3) }
          res << t.() { x = (1..3).lazy.map { _1 * 2 }; x.next; x.__send__(m) }
          res << t.() { y = ([1].each + [2]).__send__(m); [y.class.to_s, y.to_a, y.size] }
          res << t.() { y = Enumerator.product([1, 2], [3]).__send__(m); [y.class.to_s, y.to_a, y.size] }
          res << t.() { Enumerator::Generator.new { |yy| yy << 1 }.__send__(m).class.to_s }
          res << t.() { x = [1].each.freeze; y = x.__send__(m); [y.frozen?, y.to_a] }
          [Enumerator, Enumerator::Lazy, Enumerator::Chain, Enumerator::Product].each do |c|
            res << t.() { c.allocate.__send__(m) }
          end
        end
        res << t.() { x = [1, 2].each; x.next; x.rewind; y = x.dup; [y.next, x.next] }
        res << t.() { x = [1].each; x.next; (x.next rescue nil); y = x.dup; [y.next, (x.next rescue $!.class.to_s)] }
        res << t.() { x = Enumerator.new { |yy| yy << 1; raise "boom" }; x.next; (x.next rescue nil); x.dup }
        res << t.() { x = [1, 2, 3].each; y = x.dup; [y.next, y.next, x.next] }
        res << t.() { x = [1, 2].to_enum(:each) { 42 }; x.dup.size }
        res << t.() { [5, 6].each.with_index.dup.to_a }
        res << t.() { x = [1].each; x.feed(3); x.dup.next }
        res << t.() { x = [1].each; def x.hi = :hi; [x.clone.hi, (x.dup.hi rescue $!.class.to_s)] }
        res << t.() { x = Enumerator.new { |yy| yy << 1; yy << 2 }; a = x.dup; b = a.clone; [a.next, b.next, a.next, b.to_a] }
        res << t.() { x = [1].chain([2]); x.to_a; x.rewind; x.dup.to_a }
        res << t.() { x = [1].each; x.send(:initialize_copy, x).equal?(x) }
        res << t.() { [1].each.send(:initialize_copy, Object.new) }
        res << t.() { [1].each.freeze.send(:initialize_copy, [2].each) }
        res << t.() { x = [1].each; x.send(:initialize_copy, [7, 8].each); x.to_a }
        res << t.() { (1..2).lazy.send(:initialize_copy, [1].each) }
        res
    "#);
}

/// A copy of a Fiber is an uninitialized fiber, as is `Fiber.allocate`'s
/// — every method answers `FiberError: uninitialized fiber`.
#[test]
fn a_fiber_copy_is_uninitialized() {
    run(r#"
        ops = ->(f) { [f.class.to_s, (f.alive? rescue $!.message), (f.resume rescue $!.message), (f.kill rescue $!.message),
                       (f.inspect rescue $!.message), (f.to_s rescue $!.message), (f.blocking? rescue $!.message),
                       (f.storage rescue $!.message), (f.raise rescue $!.message), (f.transfer rescue $!.message)] }
        res = []
        res << t.() { ops.(Fiber.current.dup) }
        res << t.() { ops.(Fiber.current.clone) }
        res << t.() { ops.(Fiber.new { 1 }.dup) }
        res << t.() { ops.(Fiber.allocate) }
        res << t.() { f = Fiber.new { 1 }; f.instance_variable_set(:@a, 1); d = f.dup; [d.instance_variable_get(:@a), f.resume, (d.resume rescue $!.message)] }
        res << t.() { c = Class.new(Fiber); d = c.new { 1 }.dup; [d.class == c, (d.alive? rescue $!.message)] }
        res << t.() { c = Class.new(Fiber); d = c.allocate; [d.class == c, (d.alive? rescue $!.message)] }
        res
    "#);
}

/// Thread and Ractor have no allocator, so neither can be copied; the
/// thread-sync queues undefine `initialize_copy`; a Mutex copies as a
/// new, unlocked one.
#[test]
fn threads_ractors_and_queues_are_not_copied() {
    run(r#"
        res = []
        %i[dup clone].each do |m|
          res << t.() { Thread.current.__send__(m) }
          res << t.() { th = Thread.new { 1 }; th.join; th.__send__(m) }
          res << t.() { c = Class.new(Thread); th = c.new { 1 }; th.join; th.__send__(m).class }
          res << t.() { Ractor.current.__send__(m) }
          res << t.() { Thread::Queue.new.__send__(m) }
          res << t.() { Thread::SizedQueue.new(1).__send__(m) }
          res << t.() { Thread::ConditionVariable.new.__send__(m) }
          res << t.() { mu = Thread::Mutex.new; mu.lock; d = mu.__send__(m); [d.class.to_s, d.locked?, mu.locked?, d.try_lock, d.owned?] }
        end
        res << t.() { Thread.allocate }
        res << t.() { Ractor.allocate }
        res << t.() { [Ractor.main.equal?(Ractor.current), Ractor.current.class.to_s] }
        res << t.() { q = Thread::Queue.new; q << 1; [q.pop, q.respond_to?(:initialize_copy, true)] }
        res << t.() { q = Thread::SizedQueue.new(2); q << 1; q.pop }
        res
    "#);
}

