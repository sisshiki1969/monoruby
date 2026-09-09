# The `&block` parameter

How a named `&block` parameter is represented, and why. The scheme is
CRuby's (`getblockparam` / `setblockparam` / `getblockparamproxy` with the
`VM_FRAME_FLAG_MODIFIED_BLOCK_PARAM` flag), with the flag folded into the
parameter's local slot.

## The problem

A block is passed as a *block handler* in the frame (`LFP_BLOCK`): a fixnum
proxy naming the caller's block literal, a `Proc`, or a `Symbol` for `&:sym`.
`yield` and `&block` forwarding use the handler as it is. Turning it into a
`Proc` object (heap-promoting the caller's frame) is only needed when the
parameter is used as a *value*, so that is done lazily.

But the parameter is also an ordinary local variable: `block = ... unless
block`, `block ||= proc { }`, an assignment from a nested block. A reference
cannot be classified "before / after assignment" lexically (loops, one branch
of a conditional, closures created earlier), so the representation has to be
dynamic.

## The representation

Every named `&block` parameter (not an anonymous `&` or `...`) owns a local
slot, right after the parameters (`ISeqInfo::block_param_slot`). The
prologue (`InitMethod`, the VM's `fill_block_param_unset` / the JIT's
`init_func`) stores `BLOCK_PARAM_UNSET` there: an immediate no Ruby value can
be, meaning "not assigned; the frame's block handler is the value".

| operation | bytecode | what it does |
|---|---|---|
| assignment (`block = v`, from this frame or a nested block) | a plain local store | the slot leaves the sentinel |
| read as a value (`block`, `block.call`) | `BlockArg(dst, outer, slot)` | the slot's value if assigned; else the handler materialized into a `Proc` (`Executor::block_param_proc`), **cached back into `LFP_BLOCK`** so every read answers the same object (`b.equal?(b)`) |
| `&block` forwarding | `BlockArgProxy(dst, outer, slot)` | the slot's value if assigned; else the handler, a proxy re-encoded with the extra frame depth |
| `yield` / `block_given?` | unchanged | the frame's block handler, never the local (CRuby: `yield` after `b = proc {}` still calls the original block) |
| `binding.local_variable_get(:block)` | `Binding#local_variable_get` | a sentinel slot answers `block_param_proc` |

`outer` is the frame depth (a nested block reads its method's parameter),
`slot` the parameter's slot in that frame; `slot == 0` is an anonymous
parameter (no slot, no check). Bytecodegen never emits a plain slot read for
the parameter's name (`refer_local` / `refer_dynamic_local_read` answer
`None` for it), so the sentinel is never observed by Ruby code.

## Cost

Nothing changes for a method that only `yield`s. A method that names its
block pays one store at entry and one compare per reference; repeated value
reads got cheaper (one `Proc` per frame instead of one per read).

## In the JIT

`SlotState::new_method` starts the slot as the constant `C(BLOCK_PARAM_UNSET)`.
A `BlockArgProxy` / `BlockArg` whose slot the abstract state still knows as
that constant is compiled as the handler read alone (no check), and one whose
slot is a known assigned literal folds to it; only an unknown slot gets the
runtime check (`Codegen::block_arg_proxy`, both backends). The "no block
given" nil folding of a forwarded block applies when the slot is known
unassigned (which includes the anonymous `...` case it was written for).
