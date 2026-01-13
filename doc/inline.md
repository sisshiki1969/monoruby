# Inline asm function

- To avoid the overhead of method calls in performance-critical code paths, we can inline certain method calls directly into the generated machine code using inline assembly functions.
- This is particularly useful for small, frequently called methods where the overhead of a function call would be significant compared to the method's execution time.
- In inline asm functions, we have direct access to the JIT context, allowing us to manipulate the abstract state and generate machine code as needed.
- We can do 'trial inlining' by attempting to inline a method call and reverting to the original state if inlining is not possible.

## output

- accumulator(r15): result: Value

```rust
impl<'a> JitContext<'a> {
    fn inline_asm(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        f: impl Fn(
            &mut AbstractState,
            &mut AsmIr,
            &JitContext,
            &Store,
            CallSiteId,
            ClassId,
            BytecodePtr,
        ) -> bool,
        callid: CallSiteId,
        recv_class: ClassId,
        pc: BytecodePtr,
    ) -> bool {
        let state_save = state.clone();
        let ir_save = ir.save();
        if f(state, ir, self, &self.store, callid, recv_class, pc) {
            true
        } else {
            *state = state_save;
            ir.restore(ir_save);
            false
        }
    }
}
```

the signature of inline asm function is as follows:

```rust
fn(
    &mut AbstractState,
    &mut AsmIr,
    &JitContext,
    &Store,
    &CallSiteInfo,
    ClassId,
    BytecodePtr,
) -> bool
```

## inline asm function example

- We must return `true` if inlining succeeded, otherwise `false`.
- We must take arguments directly from the caller's stack using callsite information (`CallSiteId`).
- An 'inlinable' method call should have 'simple' call site, which means no keyword arguments, no splat arguments, and no block argument.
- use `AsmIr::inline()` to embed a machine code directly.
- use `AbstractState::def_rax2acc()` for moving the result (in `rax`) to the accumulator.

```rust
fn kernel_nil(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    _: ClassId,
    _: BytecodePtr,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    if state.is_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(true));
        }
    } else if state.is_not_nil(recv) {
        if let Some(dst) = dst {
            state.def_C(dst, Value::bool(false));
        }
    } else {
        state.load(ir, recv, GP::Rdi);
        ir.inline(|r#gen, _, _| {
            monoasm! { &mut r#gen.jit,
                movq rax, (FALSE_VALUE);
                movq rsi, (TRUE_VALUE);
                cmpq rdi, (NIL_VALUE);
                cmoveqq rax, rsi;
            }
        });
        state.def_rax2acc(ir, dst);
    }
    true
}
```

- Here, we check if the call site is simple. If not, we return `false` to indicate inlining failed.
- We then check the abstract state of the receiver. If we can determine it's definitely `nil` or definitely not `nil`, we set the destination accordingly.
- If we cannot determine the state of the receiver, we generate machine code to perform the check at runtime.
- Finally, we move the result from `rax` to the accumulator and return `true` to indicate successful inlining.
