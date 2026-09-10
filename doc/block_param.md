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
prologue (`InitMethod`, the VM's `clear_block_param` / the JIT's
`init_func`) clears it to 0 — `Option<Value>::None`, the same "no value was
ever put here" a not-given optional argument has — meaning "not assigned;
the frame's block handler is the value". A `Value` is never 0, so an
assignment can never be mistaken for the empty state.

| operation | bytecode | what it does |
|---|---|---|
| assignment (`block = v`, from this frame or a nested block) | a plain local store | the slot is no longer 0 |
| read as a value (`block`, `block.call`) | `BlockArg(dst, outer, slot)` | the slot's value if non-zero; else the handler materialized into a `Proc` (`Executor::block_param_proc`), **cached back into `LFP_BLOCK`** so every read answers the same object (`b.equal?(b)`) |
| `&block` forwarding | `BlockArgProxy(dst, outer, slot)` | the slot's value if non-zero; else the handler, a proxy re-encoded with the extra frame depth |
| `yield` / `block_given?` | unchanged | the frame's block handler, never the local (CRuby: `yield` after `b = proc {}` still calls the original block) |
| `binding.local_variable_get(:block)` | `Binding#local_variable_get` | an empty slot answers `block_param_proc` |

`outer` is the frame depth (a nested block reads its method's parameter),
`slot` the parameter's slot in that frame; `slot == 0` is an anonymous
parameter (no slot, no check). Bytecodegen never emits a plain slot read for
the parameter's name (`refer_local` / `refer_dynamic_local_read` answer
`None` for it), so the empty slot is never observed by Ruby code. The GC
already skips empty slots (a not-given optional argument is one too).

## Cost

Nothing changes for a method that only `yield`s. A method that names its
block pays one store at entry and one test-for-zero per reference; repeated
value reads got cheaper (one `Proc` per frame instead of one per read).

## In the JIT

The abstract state cannot hold 0 as a constant (`Value` is non-zero), and
reusing the `LinkMode::None` of a not-given optional argument would drag the
parameter into every join / bridge / write-back rule written for arguments.
So `SlotState::new_method` starts the slot as plain `S`: the memory slot is
the authority, and `BlockArgProxy` / `BlockArg` always test it at run time
(`Codegen::block_arg_proxy`, both backends), except that a literal the state
knows was assigned on the path folds to it. The "no block given" nil folding
of a forwarded block is kept only for an anonymous `&` / `...` (no slot, so
nothing can be assigned), which is the case it was written for.
