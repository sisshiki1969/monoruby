use super::*;

#[monoruby_object]
pub struct Enumerator(Value);

#[derive(Debug)]
pub struct EnumeratorInner {
    pub obj: Value,
    pub method: IdentId,
    internal: Option<Fiber>,
    /// The external-iteration driver, and the "have I been initialized
    /// yet?" flag in one.
    ///
    /// `Enumerator.allocate` (and `Enumerator::Lazy.allocate`) hand back
    /// an ENUMERATOR-typed RValue with `None` here, matching CRuby's
    /// uninitialized enumerator: the object exists and carries the right
    /// class, but every method that would iterate raises
    /// `ArgumentError: uninitialized enumerator` until `#initialize`
    /// fills it in. Allocating with the payload already in place (rather
    /// than converting a plain OBJECT in `#initialize`) keeps the ivar
    /// layout stable — an OBJECT keeps its first `OBJECT_INLINE_IVAR`
    /// ivars inside the union, so a late type change would strand them.
    pub proc: Option<Proc>,
    pub args: Box<Vec<Value>>,
    /// Keyword arguments captured at enumerator-creation time, replayed
    /// when the enumerator re-invokes `method` (e.g. `chomp:` for
    /// `String#each_line`). `None` when the source method took no
    /// keywords. See issue #742.
    pub kw_args: Option<Hashmap>,
    buffer: Option<Array>,
    /// Value parked by `#feed`, delivered as the return value of the
    /// producer's *pending* `yield` on the next `#next` / `#peek`.
    /// `Some` until the producer actually resumes past that yield, which
    /// is why a second `#feed` before then is a TypeError.
    feed: Option<Value>,
    /// Set when the producer ran to completion, holding its return value
    /// (surfaced by `StopIteration#result`). Sticky: every later `#next`
    /// re-raises StopIteration until `#rewind`, which is CRuby's
    /// `e->stop_exc`. A producer that died from an *exception* leaves it
    /// unset, so the next `#next` restarts the iteration instead.
    stop_result: Option<Value>,
    /// Optional size associated with the Enumerator:
    ///   - `None`                → unknown / fall back to method-name dispatch
    ///   - `Some(v)` with `Proc` → evaluated lazily when `#size` is called
    ///   - `Some(v)` otherwise   → Integer / Float returned as-is
    size: Option<Value>,
}

impl alloc::GC<RValue> for EnumeratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.obj.mark(alloc);
        if let Some(internal) = self.internal {
            internal.mark(alloc);
        }
        if let Some(proc) = self.proc {
            proc.mark(alloc);
        }
        self.args.iter().for_each(|v| v.mark(alloc));
        if let Some(kw) = self.kw_args {
            kw.mark(alloc);
        }
        if let Some(buf) = self.buffer {
            buf.mark(alloc)
        }
        if let Some(feed) = self.feed {
            feed.mark(alloc);
        }
        if let Some(result) = self.stop_result {
            result.mark(alloc);
        }
        if let Some(size) = self.size {
            size.mark(alloc);
        }
    }
}

impl EnumeratorInner {
    pub(crate) fn new(
        obj: Value,
        method: IdentId,
        proc: Proc,
        args: Vec<Value>,
        kw_args: Option<Hashmap>,
        size: Option<Value>,
    ) -> Self {
        Self {
            obj,
            method,
            internal: None,
            proc: Some(proc),
            args: Box::new(args),
            kw_args,
            buffer: None,
            feed: None,
            stop_result: None,
            size,
        }
    }

    /// The state `Enumerator.allocate` leaves behind: an ENUMERATOR-typed
    /// payload that has not been given a source yet. See [`Self::proc`].
    pub(crate) fn new_uninit() -> Self {
        Self {
            obj: Value::nil(),
            method: IdentId::EACH,
            internal: None,
            proc: None,
            args: Box::new(vec![]),
            kw_args: None,
            buffer: None,
            feed: None,
            stop_result: None,
            size: None,
        }
    }

    pub(crate) fn is_initialized(&self) -> bool {
        self.proc.is_some()
    }

    /// `Enumerator#initialize`: fill in (or replace) the source. Any
    /// external-iteration state from a previous initialization is
    /// dropped, so re-initializing rewinds.
    pub(crate) fn initialize(
        &mut self,
        obj: Value,
        method: IdentId,
        proc: Proc,
        args: Vec<Value>,
        kw_args: Option<Hashmap>,
        size: Option<Value>,
    ) {
        self.obj = obj;
        self.method = method;
        self.internal = None;
        self.proc = Some(proc);
        self.args = Box::new(args);
        self.kw_args = kw_args;
        self.buffer = None;
        self.feed = None;
        self.stop_result = None;
        self.size = size;
    }

    /// Raw accessor for the stored size value (before proc resolution).
    pub fn size(&self) -> Option<Value> {
        self.size
    }
}

impl Enumerator {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::ENUMERATOR));
        Self(val)
    }

    pub(crate) fn try_new(val: Value) -> Option<Self> {
        if val.ty() == Some(ObjTy::ENUMERATOR) {
            Some(Self(val))
        } else {
            None
        }
    }

    /// The receiver as an *initialized* Enumerator, or the error CRuby
    /// raises for `Enumerator.allocate.each` and friends.
    pub(crate) fn expect_initialized(val: Value) -> Result<Self> {
        match Self::try_new(val) {
            Some(e) if e.is_initialized() => Ok(e),
            Some(_) => Err(MonorubyErr::argumenterr("uninitialized enumerator")),
            None => Err(MonorubyErr::typeerr("not an Enumerator")),
        }
    }

    /// Restart external iteration. Only reachable once `#initialize`
    /// has run — every builtin entry point rejects an uninitialized
    /// enumerator first (`Enumerator::expect_initialized`).
    pub fn rewind(&mut self) {
        let proc = self.proc.expect("uninitialized enumerator");
        self.internal = Some(Fiber::from(proc));
        self.buffer = None;
        self.stop_result = None;
    }

    /// `Enumerator#rewind`: as [`Self::rewind`], but also drops a parked
    /// `#feed` value. Kept separate because `rewind` is *also* the lazy
    /// "start the producer" path, which must leave a value fed before
    /// the first `#next` in place.
    pub fn rewind_external(&mut self) {
        self.rewind();
        self.feed = None;
    }

    /// `Enumerator#feed`: park a value for the producer's pending yield.
    pub(crate) fn set_feed(&mut self, val: Value) -> Result<()> {
        if self.feed.is_some() {
            return Err(MonorubyErr::typeerr("feed value already set"));
        }
        self.feed = Some(val);
        Ok(())
    }
}

impl Enumerator {
    ///
    /// Peek next yield value from the enumerator.
    ///
    pub fn peek(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(ary) = self.buffer {
            Ok(ary.peel())
        } else {
            let ary = self.yield_next_values(vm, globals)?;
            self.buffer = Some(ary);
            Ok(ary.peel())
        }
    }

    ///
    /// Peek the next yield's values as an Array, without advancing the
    /// position. Multi-arg yields stay as arrays; a single-value yield
    /// becomes `[value]` (do NOT peel — that's what distinguishes
    /// `peek_values` from `peek`).
    ///
    pub fn peek_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        if let Some(ary) = self.buffer {
            Ok(ary)
        } else {
            let ary = self.yield_next_values(vm, globals)?;
            self.buffer = Some(ary);
            Ok(ary)
        }
    }

    ///
    /// Get next yield value from the enumerator.
    ///
    pub fn next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let v = self.next_values(vm, globals)?.peel();
        Ok(v)
    }

    ///
    /// Get next yield value from the enumerator.
    ///
    pub fn next_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        if let Some(ary) = std::mem::take(&mut self.buffer) {
            Ok(ary)
        } else {
            self.yield_next_values(vm, globals)
        }
    }

    ///
    /// Yield next value from the enumerator.
    ///
    /// If the enumerator has been exhausted, return StopIteration error.
    ///
    fn yield_next_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        // A producer that already finished keeps reporting StopIteration
        // (with its return value) until an explicit `#rewind`.
        if let Some(result) = self.stop_result {
            return Err(MonorubyErr::stopiterationerr_with_result(
                "iteration reached an end".to_string(),
                result,
            ));
        }
        if self.internal.is_none() {
            self.rewind();
        }
        let mut internal = self.internal.unwrap();
        // A fed value is the return value of the producer's *pending*
        // yield, so it is only handed over when the fiber is suspended at
        // one. A not-yet-started fiber ignores its start argument, and
        // CRuby likewise leaves `feedvalue` in place until the producer
        // resumes past a yield — which is what makes `feed; next; feed`
        // a TypeError.
        let feed = if internal.state() == FiberState::Suspended {
            self.feed.take().unwrap_or_default()
        } else {
            Value::nil()
        };
        let v = match internal.enum_yield_values(vm, globals, *self, feed) {
            Ok(v) => v,
            Err(err) => {
                // The producer blew up, leaving its fiber dead. CRuby
                // drops the fiber here so a later `#next` restarts the
                // iteration from the beginning rather than reporting
                // StopIteration forever.
                self.internal = None;
                self.buffer = None;
                return Err(err);
            }
        };
        if internal.is_terminated() {
            // `v` is the iterated method's own return value here —
            // surfaced by `StopIteration#result`.
            self.stop_result = Some(v);
            return Err(MonorubyErr::stopiterationerr_with_result(
                "iteration reached an end".to_string(),
                v,
            ));
        }
        Ok(v.as_array())
    }
}

#[monoruby_object]
pub struct Generator(Value);

impl Generator {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::GENERATOR));
        Self(val)
    }

    /// The generator body (the block `Enumerator.new` was given).
    pub(crate) fn body(&self) -> Proc {
        self.proc
    }
}

#[derive(Debug)]
pub struct GeneratorInner {
    internal: Fiber,
    proc: Proc,
    yielder: Value,
}

impl alloc::GC<RValue> for GeneratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.internal.mark(alloc);
        self.proc.mark(alloc);
        self.yielder.mark(alloc);
    }
}

impl GeneratorInner {
    pub fn new(proc: Proc) -> Self {
        let internal = Fiber::from(proc);
        Self {
            internal,
            proc,
            yielder: Value::yielder_object(),
        }
    }

    pub fn create_internal(&self) -> Fiber {
        Fiber::from(self.proc)
    }

    pub fn yielder(&self) -> Value {
        self.yielder
    }

    pub fn rewind(&mut self) {
        self.internal = self.create_internal();
    }
}
