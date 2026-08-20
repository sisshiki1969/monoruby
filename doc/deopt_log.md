# Reading the deopt log (`--features deopt`)

## What a record looks like

```
<-- deopt occurs in <Array#each> FuncId(2251).
      [:00019] %1 = %1 + %2   [Integer][Integer]   exit: deopt (chained)
      guard: monoruby/src/codegen/jitgen/asmir/compile_shared.rs:541:34
      exit emitted by: monoruby/src/codegen/jitgen/compile/method_call.rs:973:24
      cause: class version
```

| field | means |
|---|---|
| first line | the frame that deoptimized |
| `[:NNNNN]` | its bytecode index, and the TraceIR at that index |
| `exit:` | what the handler is — `deopt`, `evict`, or `recompile[reason]`, plus `(chained)` |
| `guard:` | the lowering site of the guard that **actually branched** |
| `exit emitted by:` | the front-end site that decided a deopt was needed here |
| `cause:` | the operand the guard was looking at, or a name when there is none |

`guard:` is the field to read first. It identifies the branch, not the
handler — see below for why those differ.

## Why it is built the way it is

The cause column used to be "whatever sat in rdi when the side exit ran".
That was wrong in two independent ways, and the two together produced four
consecutive misdiagnoses during the activerecord deopt investigation:

**The value was stale.** The handler read rdi *after* the deopt write-back,
which calls into C (`f64_to_val`, `create_array`, …) and clobbers the
register file. Most records printed `UNDEFINED` — how a zero word renders —
so the column was noise dressed as data.

**The guard was unidentifiable.** Deopt exits are deduplicated by
`(pc, write_back, chain)`, and a single `AsmDeopt` is handed to several
guards even before that. Neither the handler nor the `AsmDeopt` index names
the branch that was taken. Two guards at one pc are one handler, and the
log could not tell them apart.

So identity is recorded where the branch is. Every lowering site that asks
for a deopt label gets a trampoline of its own:

```
site_NNN:
    movq [rbx + EXECUTOR_DEOPT_CAUSE], <cause register>
    movl [rbx + EXECUTOR_DEOPT_SITE],  <site id>
    jmp  <deduplicated handler>
```

rbx is `&mut Executor` for the whole body, so this costs no scratch
register, no stack traffic, and — at a point where a guard has just done its
compare — no flag-clobbering instruction. Recording the operand *before* the
write-back is what makes the value trustworthy, which in turn lets the log
call stay where it always was: after the write-back, with nothing live in
registers.

Handlers stay deduplicated. Only the trampolines multiply, at ~20 cold-page
bytes each.

## The invariant

**The only way to reach a deopt handler from a JIT body is
`Codegen::deopt_label`, and it demands a `DeoptCause`.**

`SideExitLabels` has no `Index<AsmDeopt>` impl, and `deopt_label` has no
default for `cause`. A new guard that skips the question does not compile.

Choosing a cause: *for the label this call returns, is a meaningful operand
present on **every** path that jumps to it?* If some edge leaves the
register undefined, the honest answer is `DeoptCause::Static("…")`.

| variant | when |
|---|---|
| `Value(r)` | a Ruby `Value` is in `r` on every edge |
| `ValueVsBaked(r, v)` | …and the guard compared it against `v`, baked at compile time |
| `Raw(r)` | non-`Value` bits (pointers, byte counts); printed as hex, never decoded |
| `Static(s)` | no operand: global state (version word, BOP flag, counter), an unconditional deopt, or an unboxed float |

Floats are deliberately `Static`. An `FPReg` is virtual — resolving one
needs the frame's `base_stack_offset`, since a spilled float lives on the
stack rather than in an `xmm` — and threading a frame through every guard's
lowering to recover an operand for two call sites is not worth it.

## What the log will not do

**It will not decode a word it cannot vouch for.** A `Value` cause is
checked (`Value::debug_check`) before it is decoded, and the raw bits are
always printed alongside. The write-back between capture and log can run a
GC, so an object reachable only from the guard's register may be gone by
then; such a word reads as `<not a Value>` rather than as a plausible lie.

**It will not attribute a branch it did not see.** A handler entered without
a trampoline — an evict resuming through a patched return address, say —
reports `guard: unknown (handler entered without a trampoline)`. The site id
is taken and cleared on every read, so a later entry cannot inherit an
earlier one's identity.

**It will flag its own contradictions.** A `ValueVsBaked` guard that reports
a miss on bits equal to what it compares against prints
`!!! guard reported a miss on equal bits`. That exact contradiction — a
guard appearing to fail against the value it was testing for — is what cost
four hypotheses before the log could state it.

## Scope

Everything here is `cfg(feature = "deopt")`. Normal builds emit byte-identical
code: `deopt_label` compiles to the bare handler label and the `cause`
argument is discarded. `profile`-only builds keep their original call site.

aarch64 is out of scope, unchanged: `a64_gen_deopt` has never called
`log_deoptimize`, so aarch64 `deopt` builds print nothing. Adding it later is
a contained job — x19/x20/x21 mirror rbx/r12/r13, and the trampoline becomes
`str x_cause,[x19,#CAUSE]; mov w12,#id; str w12,[x19,#SITE]; b deopt`.
