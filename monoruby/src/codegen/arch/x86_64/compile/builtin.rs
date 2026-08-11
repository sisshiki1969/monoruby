//! x86-64 machine-code emitters for builtin-method JIT inliners.
//!
//! These are the assembly bodies of the `#[monoruby_builtin]` inline
//! generators (`builtins/*.rs`). The generators themselves are arch-neutral
//! (they build `AsmIr` and call one of these `emit_*` methods from inside
//! `ir.inline`); the aarch64 counterparts live in `arch/aarch64/compile.rs`
//! with the same method names.

use super::*;

impl Codegen {
    /// `BasicObject#object_id`: `i64_to_value(self_id)`; self id in rdi → rax.
    pub(crate) fn emit_object_id(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, (crate::executor::op::i64_to_value);
            call rax;
        }
    }


    /// `String#getbyte`: receiver String in rdi, fixnum index in rsi →
    /// rax = byte tagged as a fixnum, or nil when the (negative-adjusted)
    /// index is out of range.
    pub(crate) fn emit_string_getbyte(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            sarq rsi, 1;
            // rax = len, rcx = data ptr (inline vs heap storage select)
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            lea  rcx, [rdi + (RVALUE_OFFSET_INLINE)];
            cmpq rax, (STRING_INLINE_CAP);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmovgtq rcx, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            // negative index counts back from the end
            movq rdx, rsi;
            addq rdx, rax;
            testq rsi, rsi;
            cmovsq rsi, rdx;
            // unsigned bound check covers a still-negative index too
            cmpq rsi, rax;
            movq rax, (NIL_VALUE);
            jae  exit;
            movzxb rax, [rcx + rsi];
            salq rax, 1;
            orq  rax, 1;
        exit:
        }
    }

    /// `String#setbyte`: receiver String in rdi, fixnum index in rsi, fixnum
    /// byte value in rdx. Deopts when the receiver is frozen or chilled
    /// (interpreter raises / warns) or the index is out of range
    /// (interpreter raises IndexError). Keeps the cached code-range
    /// classification consistent with `RStringInner::set_byte`.
    ///
    /// ### destroy
    /// - rax, rcx, rdx, rsi
    pub(crate) fn emit_string_setbyte(&mut self, deopt: &DestLabel) {
        let exit = self.jit.label();
        let set_unknown = self.jit.label();
        let pos_idx = self.jit.label();
        monoasm! { &mut self.jit,
            // frozen (0b010) or chilled (0b100) → deopt
            movzxw rax, [rdi + (RVALUE_OFFSET_FLAG)];
            testq rax, (0b110);
            jne  deopt;
            sarq rsi, 1;
            sarq rdx, 1;
            // rax = len, rcx = data ptr (inline vs heap storage select)
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            // Shared (copy-on-write) string: the buffer is aliased by
            // other sharers and must not be written in place — deopt to
            // the interpreter, which detaches via `owned_mut`.
            // (rcx is free here until the `lea` below, so use it for the
            // tag scratch rather than r8 — r8-r11 are the allocatable pool.)
            movq rcx, (crate::rvalue::STRING_SHARED_TAG);
            cmpq rax, rcx;
            jeq  deopt;
            lea  rcx, [rdi + (RVALUE_OFFSET_INLINE)];
            cmpq rax, (STRING_INLINE_CAP);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmovgtq rcx, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            // negative index counts back from the end. Branch on the sign
            // instead of computing `rsi + rax` into a scratch reg and
            // cmov'ing, so we need no extra register (r8 is now pool).
            testq rsi, rsi;
            jns  pos_idx;
            addq rsi, rax;
        pos_idx:
            // out of range (unsigned check covers still-negative) → IndexError
            cmpq rsi, rax;
            jae  deopt;
            movb [rcx + rsi], rdx;
            // code range cache: poking an ASCII byte into a SevenBit string
            // keeps SevenBit; anything else degrades to Unknown.
            cmpb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::SevenBit as u64);
            jne  set_unknown;
            testq rdx, (0x80);
            jeq  exit;
        set_unknown:
            movb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::Unknown as u64);
        exit:
        }
    }

    /// `Hash#[]`: `hashindex(vm, globals, recv, key)`. recv in rdx, key in rcx.
    /// Result Value in rax (errors via the trailing HandleError).
    pub(crate) fn emit_hash_index(&mut self, hashindex: u64) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (hashindex);
            call rax;
        }
    }

    /// `Hash#size`: entry count of the hash in `base`, fixnum-tagged into `dst`.
    ///
    /// A small hash keeps its length in the header's representation bits; a
    /// boxed one keeps it in the entry vector. The boxed length hangs off a
    /// pointer that is only a pointer on that side of the branch, so unlike
    /// `Array#size` this cannot be a speculative load plus `cmov`.
    pub(crate) fn gen_hash_len_fixnum(&mut self, dst: GP, base: GP, layout: rubymap::EntriesLayout) {
        let (d, b) = (dst as u64, base as u64);
        let tag = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let len_off = layout.len_offset;
        monoasm! { &mut self.jit,
            // The representation bits live in the low byte of ty_flags; the
            // 32-bit load stays inside the header and `andl` clears the rest.
            movl R(d), [R(b) + (ty_flags)];
            andl R(d), (mask);
            cmpl R(d), (boxed_rep);
            jne  tag;
            movq R(d), [R(b) + (map_ptr)];
            movq R(d), [R(d) + (len_off)];
        tag:
            salq R(d), 1;
            orq  R(d), 1;
        }
    }

    /// `Hash#compare_by_identity?`: hash in `base`, Ruby bool into `dst`.
    ///
    /// Both representations reduce to one bit, so no comparison is needed:
    /// inline keeps it as a `ty_flags` bit, and the boxed `HashContent`
    /// discriminant is 0 for `Map` and 1 for `IdentMap`, so masking bit 0 of
    /// either gives the answer. `(b << 3) | FALSE_VALUE` then turns 0/1 into
    /// `false`/`true`.
    ///
    /// ### destroy
    /// - rsi
    pub(crate) fn gen_hash_compare_by_identity(&mut self, dst: GP, base: GP) {
        let (d, b) = (dst as u64, base as u64);
        let inline_case = self.jit.label();
        let tag_ready = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let ident_shift = HASH_INLINE_IDENT_BIT.trailing_zeros() as u64;
        let content = HASH_CONTENT_OFFSET;
        monoasm! { &mut self.jit,
            movl R(d), [R(b) + (ty_flags)];
            movq rsi, R(d);
            andl rsi, (mask);
            cmpl rsi, (boxed_rep);
            jne  inline_case;
            movq R(d), [R(b) + (content)];   // 0 = Map, 1 = IdentMap
            jmp  tag_ready;
        inline_case:
            shrq R(d), (ident_shift);
        tag_ready:
            andl R(d), (1);
            shlq R(d), 3;
            orq  R(d), (FALSE_VALUE);
        }
    }

    /// `Hash#default` (`want_proc == false`) / `#default_proc`: hash in `base`,
    /// result Value into `dst`.
    ///
    /// An inline hash never carries a default, a null slot means none is set,
    /// and the other discriminant belongs to the sibling accessor — all three
    /// answer `nil`, matching the builtins' `unwrap_or_default`.
    ///
    /// ### destroy
    /// - rsi
    pub(crate) fn gen_hash_default(&mut self, dst: GP, base: GP, want_proc: bool) {
        let (d, b) = (dst as u64, base as u64);
        let nil_case = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let slot = HASH_DEFAULT_OFFSET;
        let payload = HASH_DEFAULT_PAYLOAD_OFFSET;
        let want_tag = if want_proc {
            HASH_DEFAULT_TAG_PROC
        } else {
            HASH_DEFAULT_TAG_VALUE
        };
        monoasm! { &mut self.jit,
            movl rsi, [R(b) + (ty_flags)];
            andl rsi, (mask);
            cmpl rsi, (boxed_rep);
            jne  nil_case;
            movq rsi, [R(b) + (slot)];       // Option<Box<HashDefault>>: null = None
            testq rsi, rsi;
            jeq  nil_case;
            movq R(d), [rsi];                // discriminant
            cmpq R(d), (want_tag);
            jne  nil_case;
            movq R(d), [rsi + (payload)];
            jmp  exit;
        nil_case:
            movq R(d), (NIL_VALUE);
        exit:
        }
    }

    /// `Hash#__key_at` / `#__value_at`: hash in rdx, fixnum index in rcx,
    /// result Value in rax.
    ///
    /// Total by construction — a negative or out-of-range index answers `nil`
    /// rather than trapping — so there is no generic fallback and no error
    /// edge. rsi and rdi are scratch.
    pub(crate) fn gen_hash_entry_at(&mut self, want_key: bool, layout: rubymap::EntriesLayout) {
        let boxed = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let inline_field = HASH_INLINE_PAIRS_OFFSET
            + if want_key {
                HASH_INLINE_KEY_OFFSET
            } else {
                HASH_INLINE_VALUE_OFFSET
            };
        let stride = HASH_INLINE_PAIR_STRIDE;
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let len_off = layout.len_offset;
        let ptr_off = layout.ptr_offset;
        let bucket_size = layout.bucket_size;
        let bucket_field = if want_key {
            layout.key_offset
        } else {
            layout.value_offset
        };
        monoasm! { &mut self.jit,
            // nil unless one of the paths below overwrites it.
            movq rax, (NIL_VALUE);
            sarq rcx, 1;                  // untag the index
            js   exit;                    // negative → nil, as the builtin does
            movl rsi, [rdx + (ty_flags)];
            andl rsi, (mask);
            cmpl rsi, (boxed_rep);
            jeq  boxed;
            // Inline: the representation bits double as the length.
            cmpq rcx, rsi;
            jae  exit;
            movq rsi, (stride);
            imul rcx, rsi;
            addq rcx, rdx;
            movq rax, [rcx + (inline_field)];
            jmp  exit;
        boxed:
            movq rdi, [rdx + (map_ptr)];
            cmpq rcx, [rdi + (len_off)];
            jae  exit;
            movq rsi, (bucket_size);
            imul rcx, rsi;
            addq rcx, [rdi + (ptr_off)];
            movq rax, [rcx + (bucket_field)];
        exit:
        }
    }

    /// `Array#clone`: `array_clone_extern(recv)`. recv in rdi → rax.
    pub(crate) fn emit_array_clone(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rax, (f);
            call rax;
        }
    }

    /// `Array#dup`: `array_dup_extern(recv, globals)`. recv in rdi → rax.
    pub(crate) fn emit_array_dup(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rsi, r12; // globals
            movq rax, (f);
            call rax;
        }
    }

    /// `Array#<<` — append, with the no-grow case emitted inline.
    ///
    /// The receiver's class *and* its unfrozen-ness are already guarded by
    /// the caller, so the only thing separating an append from a store is
    /// spare capacity. `ArrayInner` is a `SmallVec<[Value; 5]>`, whose
    /// `capacity` field doubles as the length while the buffer is still
    /// inline (`capacity <= ARRAY_INLINE_CAPA` ⇔ not spilled), so both
    /// residencies get a two-load / one-store fast path. Only a full
    /// buffer — one append in `capacity` of them, amortized — falls
    /// through to `ary_shl` to reallocate.
    ///
    /// ### in
    /// - rdi: receiver: Array
    /// - rsi: value: Value
    ///
    /// ### out
    /// - rax: receiver: Array (`<<` returns self)
    ///
    pub(crate) fn emit_array_shl(&mut self, f: u64) {
        let heap = self.jit.label();
        let grow = self.jit.label();
        let stored = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            // Inline buffer: rax is the length, ARRAY_INLINE_CAPA the capacity.
            // The `cmpq` above already set the flags for the full-buffer test.
            jeq  grow;
            movq [rdi + rax * 8 + (RVALUE_OFFSET_INLINE)], rsi;
            addq [rdi + (RVALUE_OFFSET_ARY_CAPA)], 1;
        stored:
        }
        // Write barrier: rdi = the array (parent), rsi = the appended value.
        self.emit_write_barrier_rdi(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rax, rdi;
        exit:
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        heap:
            // Spilled buffer: rax is the capacity, the length lives beside
            // the pointer.
            movq rcx, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmpq rcx, rax;
            jge  grow;
            movq rdx, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            movq [rdx + rcx * 8], rsi;
            addq [rdi + (RVALUE_OFFSET_HEAP_LEN)], 1;
            jmp  stored;
        grow:
            // Buffer full: let `ary_shl` reallocate (and run its own barrier).
            movq rax, (f);
            call rax;
            jmp  exit;
        }
        self.jit.select_page(0);
    }

    /// `Array#[]=` slice form, `recv[start, len] = val`, with the
    /// same-length in-bounds replacement emitted inline.
    ///
    /// See `array_slice_assign` for why that shape is worth singling out: it
    /// neither grows nor shrinks the receiver, so it is a straight copy of
    /// `len` values. Everything else calls `f` (`set_array_slice`), which
    /// reproduces the builtin.
    ///
    /// ### in
    /// - rdi: receiver: Array (class- and frozen-guarded)
    /// - rsi: start: Fixnum (tagged)
    /// - rdx: val: Value
    ///
    /// ### out
    /// - rax: non-null on success (the caller's `handle_error` checks it)
    ///
    pub(crate) fn emit_array_slice_assign(&mut self, f: u64, len: usize) {
        let slow = self.jit.label();
        let src_heap = self.jit.label();
        let src_ready = self.jit.label();
        let dst_heap = self.jit.label();
        let dst_ready = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            sarq rsi, 1;                 // untag start
            js   slow;                   // negative start: let the callee wrap it
            // A self-assignment would copy a buffer over itself; hand it to
            // the callee, which snapshots the source first.
            cmpq rdi, rdx;
            jeq  slow;
            // `val` must be an Array...
            testq rdx, 0b111;
            jnz  slow;
            cmpb [rdx + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  slow;
            // ...of exactly `len` elements. r8 <- its data.
            movq rax, [rdx + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  src_heap;
            cmpq rax, (len);
            jne  slow;
            lea  r8, [rdx + (RVALUE_OFFSET_INLINE)];
        src_ready:
            // rax <- the receiver's length, r9 <- its data.
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  dst_heap;
            lea  r9, [rdi + (RVALUE_OFFSET_INLINE)];
        dst_ready:
            // The replaced run must lie inside the receiver.
            movq rcx, rsi;
            addq rcx, (len);
            cmpq rcx, rax;
            jgt  slow;
        }
        for i in 0..len {
            let disp = (i * 8) as i32;
            monoasm! { &mut self.jit,
                movq rax, [r8 + (disp)];
                movq [r9 + rsi * 8 + (disp)], rax;
            }
        }
        // Several children stored at once: remember the receiver wholesale.
        self.emit_write_barrier_bulk_rdi();
        monoasm! { &mut self.jit,
            movq rax, rdx;               // `[]=` evaluates to the assigned value
            jmp  exit;
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        src_heap:
            movq rcx, [rdx + (RVALUE_OFFSET_HEAP_LEN)];
            cmpq rcx, (len);
            jne  slow;
            movq r8, [rdx + (RVALUE_OFFSET_HEAP_PTR)];
            jmp  src_ready;
        dst_heap:
            movq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            movq r9, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            jmp  dst_ready;
        slow:
            // set_array_slice(base, start, len, val, vm, globals).
            // rdi = base and rsi = start are already in place.
            movq rcx, rdx;               // val  -> arg3
            movq rdx, (len);             // len  -> arg2
            movq r8, rbx;                // vm
            movq r9, r12;                // globals
            movq rax, (f);
            call rax;
            jmp  exit;
        }
        self.jit.select_page(0);
        self.jit.bind_label(exit);
    }

    /// `Array#rotate!`: `ary_rotate_(recv, count)`. recv in rdi; the count
    /// arrives tagged in rsi (or is the implicit `1`), and the callee takes a
    /// plain `i64`. → rax.
    pub(crate) fn emit_array_rotate_(&mut self, f: u64, has_arg: bool) {
        if has_arg {
            monoasm! { &mut self.jit, sarq rsi, 1; }
        } else {
            monoasm! { &mut self.jit, movq rsi, 1; }
        }
        monoasm! { &mut self.jit,
            movq rax, (f);
            call rax;
        }
    }

    /// `Class#allocate`: `alloc_func(class_id, globals)` → rax.
    ///
    /// With an `inline` payload the whole allocation is emitted here
    /// instead: pop a cell (`emit_alloc_cell`) and write exactly what the
    /// stock allocator would have produced. The runtime call is kept as the
    /// fallback for the page-boundary cases.
    pub(crate) fn emit_class_allocate(
        &mut self,
        class_id: u32,
        alloc_func: u64,
        inline: Option<InlineAlloc>,
    ) {
        let Some(inline) = inline.filter(|_| !self.alloc_free_head_addr.is_null()) else {
            self.class_allocate_call(class_id, alloc_func);
            return;
        };
        let slow = self.jit.label();
        let cont = self.jit.label();
        // 8-byte object header: flag=1 (live) | ty<<16 | class<<32.
        let ty = match inline {
            InlineAlloc::Object => ObjTy::OBJECT,
            InlineAlloc::Struct(_) => ObjTy::STRUCT,
        };
        let header: u64 = ((class_id as u64) << 32) | ((ty.get() as u64) << 16) | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm! { &mut self.jit,
            movq [rax + (RVALUE_OFFSET_VAR)], 0;  // var_table = None
        }
        match inline {
            // `ObjKind::object()` == `[None; OBJECT_INLINE_IVAR]` at the
            // head of the `kind` union (the same `RVALUE_OFFSET_KIND +
            // ivarid * 8` addressing the ivar emitters use), and `None`
            // for `Option<Value>` is a zero word.
            InlineAlloc::Object => {
                for k in 0..OBJECT_INLINE_IVAR {
                    let off = RVALUE_OFFSET_KIND as i32 + (k as i32) * 8;
                    monoasm! { &mut self.jit,
                        movq [rax + (off)], 0;
                    }
                }
            }
            // `StructInner::new(len)` == a `SmallVec` holding `len` nils:
            // the smallvec's capacity field doubles as the inline length,
            // and slots past `len` stay untouched, exactly as in Rust.
            InlineAlloc::Struct(len) => {
                monoasm! { &mut self.jit,
                    movq [rax + (RVALUE_OFFSET_ARY_CAPA)], (len as i32);
                }
                for k in 0..len {
                    let off = RVALUE_OFFSET_INLINE as i32 + (k as i32) * 8;
                    monoasm! { &mut self.jit,
                        movq [rax + (off)], (NIL_VALUE as i32);
                    }
                }
            }
        }
        monoasm! { &mut self.jit,
            jmp  cont;
        slow:
        }
        self.class_allocate_call(class_id, alloc_func);
        monoasm! { &mut self.jit,
        cont:
        }
    }

    fn class_allocate_call(&mut self, class_id: u32, alloc_func: u64) {
        monoasm! { &mut self.jit,
            movl rdi, (class_id);
            movq rsi, r12;
            movq rax, (alloc_func);
            call rax;
        }
    }


    /// `Float#to_i`: truncate `fsrc` to i64, tag as fixnum in rdi, deopt on
    /// out-of-fixnum overflow.
    pub(crate) fn emit_float_to_int(&mut self, fsrc: FPReg, deopt: &DestLabel, base: usize) {
        self.load_fpr_into_xmm0(fsrc, base);
        monoasm! { &mut self.jit,
            cvttsd2siq rdi, xmm0;
            addq  rdi, rdi;
            jo    deopt;
            orq   rdi, 1;
        }
    }

    /// `Fiber.yield` with no args: the yielded value (rsi) is nil.
    pub(crate) fn emit_fiber_yield_value_nil(&mut self) {
        monoasm! { &mut self.jit,
            movq rsi, (Value::nil().id());
        }
    }

    /// `Fiber.yield(*args)` with ≥2 args: build the args array, leaving it in
    /// rsi. `args_off` is `conv(args)`.
    pub(crate) fn emit_fiber_yield_value_array(&mut self, args_off: usize, pos_num: usize) {
        monoasm! { &mut self.jit,
            lea rdi, [r14 - (args_off as i32)];
            movq rsi, (pos_num);
            movq rax, (crate::runtime::create_array);
            call rax;
            movq rsi, rax;
        }
    }

    /// `Fiber.yield`: call `yield_fiber(vm, value)` (value already in rsi).
    /// A yield with no parent fiber (main fiber / a green thread's root)
    /// must not reach the switch stub — it would load rsp through a null
    /// `parent_fiber` — so route it to the error helper instead (returns
    /// None with a FiberError set; the inline's handle_error picks it up).
    pub(crate) fn emit_fiber_yield_call(&mut self, yield_fiber: u64, no_parent: u64) {
        let none = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            cmpq [rdi + (EXECUTOR_PARENT_FIBER)], 0;
            jeq  none;
            movq rax, (yield_fiber);
            call rax;
            jmp  exit;
        none:
            movq rax, (no_parent);
            call rax;
        exit:
        }
    }

    /// Load a 64-bit tagged fixnum literal into rsi (the RHS register) for an
    /// `Integer` bit-op whose immediate doesn't fit a 32-bit encoding.
    pub(crate) fn emit_load_tagged_rsi(&mut self, tagged: i64) {
        monoasm!( &mut self.jit, movq rsi, (tagged); );
    }

    /// `Integer#|` with a tagged immediate (`(2a+1)|(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitor_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, orq rdi, (imm); );
    }
    /// `Integer#|` register-register.
    pub(crate) fn emit_bitor_rr(&mut self) {
        monoasm!( &mut self.jit, orq rdi, rsi; );
    }
    /// `Integer#&` with a tagged immediate (`(2a+1)&(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitand_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, andq rdi, (imm); );
    }
    /// `Integer#&` register-register.
    pub(crate) fn emit_bitand_rr(&mut self) {
        monoasm!( &mut self.jit, andq rdi, rsi; );
    }
    /// `Integer#^` with a tagged immediate (use `imm-1` so lhs's tag survives).
    pub(crate) fn emit_bitxor_imm(&mut self, imm: i64) {
        monoasm!( &mut self.jit, xorq rdi, (imm - 1); );
    }
    /// `Integer#^` register-register (`(2a+1)^(2b+1)` clears LSB, re-tag +1).
    pub(crate) fn emit_bitxor_rr(&mut self) {
        monoasm!( &mut self.jit,
            xorq rdi, rsi;
            addq rdi, 1;
        );
    }

    /// `n << k` / `n >> -k` with `k >= 64`: a non-zero `n` overflows (deopt);
    /// `0` shifts to `0`. lhs in rdi.
    pub(crate) fn emit_shl_overflow_zero(&mut self, z: i64, deopt: &DestLabel) {
        monoasm!( &mut self.jit,
            cmpq rdi, (z);
            jne deopt;
            movq rdi, (z);
        );
    }

    /// `Fiddle.___read` integer load: untag the pointer in rdi, deopt on NULL,
    /// load a `width`-byte value (sign/zero-extended per `signed`), tag the
    /// result as a fixnum in rax.
    pub(crate) fn emit_fiddle_read_int(&mut self, width: u8, signed: bool, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
        }
        match (width, signed) {
            (1, true) => monoasm! { &mut self.jit, movsxb rax, [rdi]; },
            (1, false) => monoasm! { &mut self.jit, movzxb rax, [rdi]; },
            (2, true) => monoasm! { &mut self.jit, movsxw rax, [rdi]; },
            (2, false) => monoasm! { &mut self.jit, movzxw rax, [rdi]; },
            (4, true) => monoasm! { &mut self.jit, movsxl rax, [rdi]; },
            (4, false) => monoasm! { &mut self.jit, movl rax, [rdi]; },
            _ => unreachable!(),
        }
        // Tag as Fixnum: rax = (rax << 1) | 1.
        monoasm! { &mut self.jit,
            addq rax, rax;
            orq rax, 1;
        }
    }

    /// `Fiddle.___read` f64 load: untag the pointer in rdi, deopt on NULL, load
    /// the double into `fret`.
    pub(crate) fn emit_fiddle_read_f64(&mut self, fret: FPReg, deopt: &DestLabel, base: usize) {
        monoasm! { &mut self.jit,
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            movq xmm0, [rdi];
        }
        self.store_fpr_into_xmm(fret, base);
    }

    /// `Fiddle.___write` integer store: save the tagged pointer (the return
    /// value) in rax, untag the pointer in rdi, deopt on NULL, untag the value
    /// in rsi and store its low `width` bytes.
    pub(crate) fn emit_fiddle_write_int(&mut self, width: u8, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            movq rax, rdi;
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            sarq rsi, 1;
        }
        match width {
            1 => monoasm! { &mut self.jit, movb [rdi], rsi; },
            2 => monoasm! { &mut self.jit, movw [rdi], rsi; },
            4 => monoasm! { &mut self.jit, movl [rdi], rsi; },
            _ => unreachable!(),
        }
    }

    /// `Fiddle.___write` f64 store: load the source double into xmm0, save the
    /// tagged pointer in rax, untag the pointer in rdi, deopt on NULL, store the
    /// double.
    pub(crate) fn emit_fiddle_write_f64(&mut self, xsrc: FPReg, deopt: &DestLabel, base: usize) {
        self.load_fpr_into_xmm0(xsrc, base);
        monoasm! { &mut self.jit,
            movq rax, rdi;
            sarq rdi, 1;
            testq rdi, rdi;
            jz deopt;
            movq [rdi], xmm0;
        }
    }

    /// `Integer#%` by a positive power of two: `lhs & mask` on the tagged
    /// fixnum in rdi.
    pub(crate) fn emit_int_rem_pow2_mask(&mut self, mask: i64) {
        if let Ok(imm32) = i32::try_from(mask) {
            let imm = imm32 as i64;
            monoasm!( &mut self.jit, andq rdi, (imm); );
        } else {
            monoasm!( &mut self.jit, movq rax, (mask); andq rdi, rax; );
        }
    }
}
