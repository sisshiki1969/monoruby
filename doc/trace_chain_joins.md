# Trace-chain joins — one `AbstractState` for the whole specialization stack

**Kind: design record.** Why the JIT's abstract state spans every frame of
the trace being compiled — suspended method callers included — and how the
merge machinery (join / equiv / bridge), specialized returns, and the
outer-frame float claims all ride that one chain. This records the design
as landed and the alternatives that were tried and rejected on the way.

The observation driving it: *joining the frames suspended mid-trace and
joining the lexical outer scopes are the same operation.* Every mechanism
below is an instance of letting the ordinary state machinery — clone at a
branch, join at a merge, bridge the difference — do a job that previously
had a bespoke side channel.

## 1. The chain

`AbstractState` holds a `Vec<AbstractFrame>`, 1:1 with `JitContext`'s
specialization stack (`stack_frame`): the unit root at position 0, the
innermost (currently compiling) frame last. This is the *trace* chain,
not the lexical chain — a specialized method caller like `Integer#times`
sits in it even though it is nobody's lexical outer. Each frame carries
`lexical_outer: Option<usize>` (the state-side twin of
`JitStackFrame::outer`); dynvar addressing walks those links
(`outer_level`), while joins, `equiv`, and bridges walk every frame of
the chain regardless of lexical reachability.

A nested specialized compile enters with the caller's live chain
(`specialized_compile` passes `state.frames_cloned()` to
`AbstractState::with_chain`), so what the callee's compile believes about
the outer frames is exactly what this call path believed at the call —
per branch, not a checkpoint from frame entry.

## 2. Returns are merge edges

A plain `Ret` in a specialized callee is not a terminator; it is an entry
edge of the caller's continuation. Each one branches to an outline return
segment recording the full chain at that return
(`record_return_edge`); after the callee's body is compiled,
`build_return_segments` joins the edges' chains (`join_no_alloc`) and
emits, per edge, the bridge from that edge's state to the join, the
return-value load, and the actual `Ret`.

The bridge is where the kept-constant discipline becomes per-path. A
caller may keep a `C(v)` claim on one of its slots across the call (the
slot unwritten, the constant folded into the callee); a return path on
which the callee's subtree gave the claim up meets `C ⊔ S → S` at the
join, and that path's segment emits the literal write
(chain-addressed `StoreDynVarSpecialized`) — the *surrender write* — so
every resuming path arrives with the slot current. A path that kept the
claim writes nothing. Before this, keeping a constant required the claim
to survive *every* path or be given up everywhere.

## 3. The resume asymmetry

When `specialized_compile` returns, the caller resumes from the join of
the return-path chains — with one deliberate asymmetry, learned by
bisection (see §7):

- **Outer levels** (everything below the caller) take the joined chains
  whole. Their own-timeline bookkeeping is not consumed here; each
  becomes "the resuming level" only at its own caller's resume, where
  this same rule applies.
- **The caller level** keeps the frame it parked at the call (its pc,
  liveness, hints, and invariant flags belong to its own compile
  timeline) and overlays only the slot claims the join validated
  (`overlay_kept_constants`): kept `C`s (sound because every return path
  either held the claim or emitted its surrender write), spill-homed
  `Sf(Float)` promotions the subtree made, and the monotone
  `subtree_float_read` mark bits.

If the callee compiled **no** plain `Ret` (every path raises, breaks, or
returns non-locally), the caller's continuation is unreachable on
compiled paths. The resume state is then rebuilt from a copy of the
call's own entry chain, re-widened by the callee-era delta of the
`widened_outer_log`. Any claim that placeholder still carries is
vacuous — no execution arrives through it — and a later merge's claims
are established by its *reachable* entries' bridges.

## 4. Invariants travel lexically on the live chain

The frame-chain no-capture invariant (`no_capture_guard`, licensing
static chain addressing) is maintained on the live chain by a lexical
walk: a call that may capture unsets it up the lexical links
(`unset_lexical_no_capture_guard`), and the capture guard emitted after
the call re-proves it up the same links
(`set_lexical_no_capture_guard`) — sound because
`branch_if_captured`'s meta check also catches an ancestor
`move_frame_to_heap` (a captured ancestor tombstones the frame). The
first cut unset the invariant on *every* chain frame but re-proved only
the innermost, which one-way-ratcheted the outer frames' invariants
false and silently degraded every chain-addressed load behind them back
to the generic walk — found by instruction-count probes, +22% on the
block-read benchmarks.

## 5. Outer-float claims as ordinary state

The outer-frame float roadmap (write-through keeps, store-driven `S→Sf`
promotion with spill homes, home-directed reads, home-aliased bare-`F`
reads, and loop-entry adoption) originally kept several side channels on
the parked frame copies. Each has since become ordinary state:

- **Read decisions** consult the live chain (`outer_sf_float`,
  `outer_no_capture_guard` on `AbstractState`).
- **Float-read marks** (stage A: an inlined callee consumed an outer
  slot's value as a raw f64) land on the owner's frame *in the state*
  (`mark_outer_float_read`) and travel like any monotone hint — ORed
  through joins, carried by return chains, merged by the resume overlay.
- **Loop-entry adoption** (stage C) reads the marks off the loop's
  back-edge state at the merge, minus what the incoming state already
  had; the loop analysis exports only its *vetoes* (the claim barrier /
  generic-yield poison and the outer widens its walk performed). The
  adopted `Sf(Float)` binds on the live loop-entry state; its scope is
  handled by the joins themselves — a path bypassing the loop head meets
  the claim back to `S` — so no revert machinery exists.

## 6. What the chain-wide join subsumed

`converge_block_entry` — a probe fixpoint that compiled a yielded block
into a throwaway context to discover which outer constants it gives up,
treating "this block may be entered again" as a back edge — was deleted
outright. Every re-entry path is a join the chain already covers:

- a yield inside the compiling unit's own loop demotes outer `C`s at
  that loop's back-edge join (`analyse_backedge_fixpoint` joins every
  frame of the chain);
- straight-line repeated yields thread the widen through the live chain
  from one yield to the next;
- a re-entry through a loop *outside* the chain can only be a generic
  yield, where the caller's `forget_constants` bet-confirmation drops
  every claim.

With it went the last cross-frame *readers* of the parked copies'
constant claims (`outer_const_count`, `adopt_outer_widenings`,
`lost_constants_of`) — and one probe compile per specialized yield.

## 7. What the parked copy still is

`JitStackFrame.abstract_state` — the frame a compile parks when it
suspends for a nested compile — is no longer a truth channel for
cross-frame claims. What remains:

- **The suspended frame's own resume record.** The caller level resumes
  from it (§3). Subtree events that must reach that record do so under
  strict monotone rules: widens (`widen_outer_at_pos`, dual-written with
  the live chain and logged), capture unsets, and the stage-2 `Sf`
  promotion binding.
- **The spill-home id space.** A frame's spill-resident FPR ids must be
  unique across its whole life — its own compile segments and every
  claim a nested compile makes while it is suspended — and the parked
  file's allocator is that single id space. Loop-entry adoption reserves
  ids there without binding (`reserve_spill_home`); stage-2 promotion
  allocates and binds (`alloc_spill_home`).
- **The stage-2 gate** (`try_promote_outer_sf` requires the owner's
  parked mode to still be a plain `S`).

Never publish a nested compile's view of a frame *into* its parked slot:
that replaces the state the suspended frame will resume from with a view
taken at a different program point, in a different frame's terms — three
levels of nested blocks segfaulted in generated code when this was
tried. Retiring the remaining allocator/gate roles would need an
allocator that lives outside the frame's state (with a resume-time sync
into the owner's own allocator); that is allocator engineering, distinct
from the join semantics this document records.

## 8. Rejected alternatives

- **Full-chain replacement at resume.** Replacing the caller level with
  the callee's joined view of it broke real code (`Integer#downto`
  resumed a `C(0)` where the parked frame held a runtime value;
  `Array#permutation` read `nil`). Bisection modes that kept the parked
  caller level and overlaid only validated claims passed — hence §3's
  asymmetry. The underlying reason: `AbstractFrame`/`SlotState`
  interleave slot *claims* with own-timeline metadata (pc, liveness,
  guard hints, the FPR allocator), and only the claims are validated by
  the return join.
- **Marks inside the `IsUsed` lattice.** Folding the stage-A float-read
  marks into the existing use/type lattice perturbed tuned owner
  policies (+7% on mandelbrot); the marks are a separate bitvec with a
  single consumer instead.
- **Invariants restored from the parked copy at nested entry.** Worked,
  but treated the symptom of the one-way ratchet (§4); replaced by the
  lexical unset/re-prove pair, after which the restoration was removed.
- **Capture events as an adoption barrier.** Not needed: the runtime
  capture guard (with its tombstone check, §4) already covers ancestor
  promotions, so stage-C adoption gates on the invariant rather than
  poisoning the whole loop.
