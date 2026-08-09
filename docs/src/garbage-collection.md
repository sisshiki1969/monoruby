monoruby has its own garbage collector: a **non-moving, single-threaded, stop-the-world, generational mark-and-sweep** collector, modeled on CRuby's RGenGC. This page is an overview; the full design document is [`doc/gc.md`](design/gc.md), and the implementation lives in [`monoruby/src/alloc.rs`](https://github.com/sisshiki1969/monoruby/blob/master/monoruby/src/alloc.rs).

## Heap layout

- All heap objects (`RValue`) are exactly **64 bytes**. Memory comes from a single 2 GB virtual arena reserved up front, carved into **256 KB pages** of 4032 cells each.
- Mark bits and old bits are stored **outside** the object cells, as per-page bitmaps. A pointer's page is found with a single address mask, so bitmap lookup is O(1).
- Objects **never move**, so raw `*const RValue` pointers stay valid across collections.
- Allocation pops from a free list when possible, otherwise bump-allocates in the current page. The JIT inlines this free-list fast path directly into compiled code.

## Generational collection

Minor collections trace only young objects; old objects are assumed live and their mark bits are seeded from the old bitmap.

- Objects of promotable types (Object, String, Array, Hash, Bignum, Float, Struct) age by one on each minor GC they survive; at **age 3** they are promoted to the old generation.
- **Write barrier**: when a reference is stored into an old object, a single header-bit test decides whether the object must enter the **remembered set**, whose old→young edges are traced during minor GCs. The JIT emits the barrier inline; bulk operations (`Array#concat`, …) use a bulk variant.
- A **major** (full) collection runs when the old-object count crosses an adaptive threshold or after 64 consecutive minors; it clears all generation state and retraces everything. `GC.start` always forces a major.

![Object state transitions](design/gc_state_transitions.svg)

The write-barrier / remembered-set interaction (including why a minor GC without the barrier would sweep live objects) is illustrated in [`doc/gc_write_barrier.svg`](design/gc_write_barrier.svg).

## GC triggers and safepoints

Collections are requested by setting a single per-thread `alloc_flag`, and *performed* only at **safepoints**:

- **Allocation pressure** — every ~8 filled pages trips the flag.
- **malloc pressure** — a custom `#[global_allocator]` tracks off-heap allocation (String/Array backing stores etc.) and requests a GC when it outgrows an adaptive threshold, so heavy malloc traffic can't outrun the heap-cell trigger.
- **Explicit** — `GC.start`.

Safepoint polls (`compare alloc_flag; conditionally call gc`) are emitted at **callee entry and loop back-edges** in both the VM tier and JIT code. The same poll also drives green-thread preemption and pending-signal delivery (see [Threads and Fibers](threads-and-fibers.md)). Rooting is **precise**: roots are explicitly enumerated — the executor's frame chain, temporary-value stack, the green-thread scheduler's thread registry, pending exceptions, and global state — never conservatively scanned off the machine stack; JIT code spills live registers before a safepoint call.

## Controlling the GC

| Control | Effect |
| --- | --- |
| `GC.start` | Force a full (major) collection |
| `GC.enable` / `GC.disable` | Toggle collection at runtime |
| `--no-gc` CLI flag | Disable GC for the process |
| `GC.count` / `GC.stat` | Collection counters / CRuby-compatible stats |

Debugging Cargo features: `gc-log` (stats at exit), `gc-debug` (assertions), `gc-stress` (start in `GC.stress` mode and collect at every safepoint — used by `bin/test`'s nextest phase on x86-64 CI), `gc-verify` (independent re-mark verification after each minor GC).

## Further reading

- [`doc/gc.md`](design/gc.md) — full design document (heap layout, bitmaps, aging/promotion, remembered-set self-cleaning, heap-escaped frame reclamation)
- [`doc/safepoint.md`](design/safepoint.md) — the safepoint / poll-flag mechanism shared by GC, preemption, and signals
