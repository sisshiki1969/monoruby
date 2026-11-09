//!
//! Per-method compile time inside one compilation unit (`jit-log`).
//!
//! A unit compiles its root method together with every specialized (and
//! frameless-inlined) callee, so the unit's elapsed time says little about
//! any one method. Both halves of the compiler recurse into the callees —
//! the front-end (`traceir_to_asmir`, TraceIR → AsmIR) from the call site
//! being compiled, the back-end (`gen_machine_code`, AsmIR → machine code)
//! from the frame that owns the callee — so each half is timed as a stack:
//! a frame's time is its *exclusive* time, what its own invocation took
//! minus the invocations nested inside it. Summed per (method, self class),
//! the parts add up to the unit's front-end and back-end time.
//!
//! The front-end of one callee can run more than once in a unit: it is
//! specialized at every call site separately, and the loop back-edge
//! fixpoint walks a loop body (and the callees in it) until it converges.
//! A front-end run whose result is thrown away (an aborted specialization)
//! is counted too, since it is compile time all the same. `front_runs` /
//! `back_runs` say how many runs a row sums.
//!
//! The stack lives in a thread-local: a compile runs start to finish on one
//! thread and never re-enters itself.
//!

use super::*;
use std::cell::RefCell;
use std::time::{Duration, Instant};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Phase {
    /// TraceIR → AsmIR (`JitContext::traceir_to_asmir`).
    FrontEnd,
    /// AsmIR → machine code (`Codegen::gen_machine_code`).
    BackEnd,
}

struct Open {
    key: (ISeqId, ClassId),
    phase: Phase,
    start: Instant,
    /// Time of the runs nested inside this one, all phases.
    nested: Duration,
}

/// The compile time of one (method, self class) within a unit.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct MethodTime {
    pub iseq_id: ISeqId,
    pub self_class: ClassId,
    pub front: Duration,
    pub back: Duration,
    pub front_runs: u32,
    pub back_runs: u32,
}

impl MethodTime {
    fn total(&self) -> Duration {
        self.front + self.back
    }
}

#[derive(Default)]
struct Timer {
    stack: Vec<Open>,
    /// In the order their first run finished.
    methods: Vec<MethodTime>,
}

impl Timer {
    fn enter(&mut self, key: (ISeqId, ClassId), phase: Phase, now: Instant) {
        self.stack.push(Open {
            key,
            phase,
            start: now,
            nested: Duration::ZERO,
        });
    }

    fn exit(&mut self, now: Instant) {
        let open = self.stack.pop().expect("jit_timing: exit without enter");
        let inclusive = now.saturating_duration_since(open.start);
        if let Some(parent) = self.stack.last_mut() {
            parent.nested += inclusive;
        }
        let exclusive = inclusive.saturating_sub(open.nested);
        let (iseq_id, self_class) = open.key;
        let i = match self
            .methods
            .iter()
            .position(|m| (m.iseq_id, m.self_class) == open.key)
        {
            Some(i) => i,
            None => {
                self.methods.push(MethodTime {
                    iseq_id,
                    self_class,
                    front: Duration::ZERO,
                    back: Duration::ZERO,
                    front_runs: 0,
                    back_runs: 0,
                });
                self.methods.len() - 1
            }
        };
        let m = &mut self.methods[i];
        match open.phase {
            Phase::FrontEnd => {
                m.front += exclusive;
                m.front_runs += 1;
            }
            Phase::BackEnd => {
                m.back += exclusive;
                m.back_runs += 1;
            }
        }
    }
}

thread_local! {
    static TIMER: RefCell<Timer> = RefCell::new(Timer::default());
}

/// Start a unit: forget whatever an earlier unit left behind.
pub(crate) fn begin_unit() {
    TIMER.with(|t| *t.borrow_mut() = Timer::default());
}

/// Run *f* as one *phase* run of (*iseq_id*, *self_class*).
pub(crate) fn time<R>(
    iseq_id: ISeqId,
    self_class: ClassId,
    phase: Phase,
    f: impl FnOnce() -> R,
) -> R {
    TIMER.with(|t| {
        t.borrow_mut()
            .enter((iseq_id, self_class), phase, Instant::now())
    });
    let r = f();
    TIMER.with(|t| t.borrow_mut().exit(Instant::now()));
    r
}

/// End a unit: the per-method times, in the order their first run finished.
pub(crate) fn take_unit() -> Vec<MethodTime> {
    TIMER.with(|t| {
        let mut t = t.borrow_mut();
        debug_assert!(t.stack.is_empty(), "jit_timing: unbalanced runs");
        t.stack.clear();
        std::mem::take(&mut t.methods)
    })
}

/// The per-method table `jit-log` prints after a unit, largest first.
/// *elapsed* is the unit's whole compile; what the two phases do not cover
/// (finalize, patching, bookkeeping) is shown as `other`.
pub(crate) fn format_unit(store: &Store, methods: &[MethodTime], elapsed: Duration) -> String {
    let front: Duration = methods.iter().map(|m| m.front).sum();
    let back: Duration = methods.iter().map(|m| m.back).sum();
    let other = elapsed.saturating_sub(front + back);
    let mut buf = format!(
        "    compile time by method (callees excluded): unit {elapsed:?} = front-end {front:?} + back-end {back:?} + other {other:?}\n"
    );
    buf += "      total       front-end    back-end     runs(f/b)  method\n";
    let mut sorted: Vec<&MethodTime> = methods.iter().collect();
    sorted.sort_by(|a, b| b.total().cmp(&a.total()));
    for m in sorted {
        let iseq = &store[m.iseq_id];
        buf += &format!(
            "      {:<11} {:<12} {:<12} {:>4}/{:<4}  [{:?}] <{}> self_class:{} {}:{}\n",
            format!("{:?}", m.total()),
            format!("{:?}", m.front),
            format!("{:?}", m.back),
            m.front_runs,
            m.back_runs,
            m.iseq_id,
            store.func_description(iseq.func_id()),
            store.debug_class_name(m.self_class),
            iseq.sourceinfo.file_name(),
            iseq.sourceinfo.get_line(&iseq.loc),
        );
    }
    buf
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ms(n: u64) -> Duration {
        Duration::from_millis(n)
    }

    #[test]
    fn nested_runs_are_subtracted_from_their_parent() {
        let root = (ISeqId::new(1), OBJECT_CLASS);
        let callee = (ISeqId::new(2), OBJECT_CLASS);
        let t0 = Instant::now();
        let mut t = Timer::default();
        // root front-end 0..10, with the callee's front-end 2..5 and,
        // again at a second call site, 6..7 nested inside it.
        t.enter(root, Phase::FrontEnd, t0);
        t.enter(callee, Phase::FrontEnd, t0 + ms(2));
        t.exit(t0 + ms(5));
        t.enter(callee, Phase::FrontEnd, t0 + ms(6));
        t.exit(t0 + ms(7));
        t.exit(t0 + ms(10));
        // back-end: root 10..20 with the callee's 12..16 nested.
        t.enter(root, Phase::BackEnd, t0 + ms(10));
        t.enter(callee, Phase::BackEnd, t0 + ms(12));
        t.exit(t0 + ms(16));
        t.exit(t0 + ms(20));
        assert!(t.stack.is_empty());
        let find = |iseq_id| t.methods.iter().find(|m| m.iseq_id == iseq_id).unwrap();
        let r = find(root.0);
        assert_eq!((r.front, r.back), (ms(6), ms(6)));
        assert_eq!((r.front_runs, r.back_runs), (1, 1));
        let c = find(callee.0);
        assert_eq!((c.front, c.back), (ms(4), ms(4)));
        assert_eq!((c.front_runs, c.back_runs), (2, 1));
        // The exclusive times add up to the unit's 20ms.
        let sum: Duration = t.methods.iter().map(|m| m.total()).sum();
        assert_eq!(sum, ms(20));
    }

    #[test]
    fn same_method_with_another_self_class_is_its_own_row() {
        let t0 = Instant::now();
        let mut t = Timer::default();
        t.enter((ISeqId::new(1), OBJECT_CLASS), Phase::FrontEnd, t0);
        t.enter((ISeqId::new(1), INTEGER_CLASS), Phase::FrontEnd, t0);
        t.exit(t0 + ms(1));
        t.exit(t0 + ms(3));
        assert_eq!(t.methods.len(), 2);
        assert_eq!(t.methods[0].self_class, INTEGER_CLASS);
        assert_eq!(t.methods[0].front, ms(1));
        assert_eq!(t.methods[1].front, ms(2));
    }
}
