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

    /// `Object#frozen?`: receiver Value in rdi → rax = true/false Value.
    /// Mirrors `Value::is_frozen`: packed values and heap Numerics
    /// (Bignum / Float / Complex / Rational) are always frozen; every other
    /// heap object tests the header FROZEN bit (bit 1). Chilled strings have
    /// that bit clear, so they report false, exactly like the builtin.
    ///
    /// ### destroy
    /// - rax, rcx
    pub(crate) fn emit_frozen_pred(&mut self) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, (TRUE_VALUE);
            testq rdi, (0b111);
            jne  exit;                                   // packed -> frozen
            movzxb rcx, [rdi + (RVALUE_OFFSET_TY)];
            cmpb rcx, (ObjTy::BIGNUM.get());
            jeq  exit;
            cmpb rcx, (ObjTy::FLOAT.get());
            jeq  exit;
            cmpb rcx, (ObjTy::COMPLEX.get());
            jeq  exit;
            cmpb rcx, (ObjTy::RATIONAL.get());
            jeq  exit;
            testb [rdi + (RVALUE_OFFSET_FLAG)], (0b10);  // FROZEN bit
            jne  exit;
            movq rax, (FALSE_VALUE);
        exit:
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
    /// (interpreter raises IndexError). A shared (copy-on-write) receiver is
    /// detached in place via `runtime::str_detach` and retried — NOT deopted:
    /// `s = lit.dup; s.setbyte(..)` makes the shared miss chronic (once per
    /// dup'd string), and with side-exit escalation unconditional each deopt
    /// walks and converts the whole caller chain (the ruby-xor regression).
    /// Keeps the cached code-range classification consistent with
    /// `RStringInner::set_byte`.
    ///
    /// ### destroy
    /// - rax, rcx, rdx, rsi
    pub(crate) fn emit_string_setbyte(&mut self, deopt: &DestLabel) {
        let exit = self.jit.label();
        let set_unknown = self.jit.label();
        let pos_idx = self.jit.label();
        let reload = self.jit.label();
        let shared = self.jit.label();
        let done = self.jit.label();
        monoasm! { &mut self.jit,
            // frozen (0b010) or chilled (0b100) → deopt
            movzxw rax, [rdi + (RVALUE_OFFSET_FLAG)];
            testq rax, (0b110);
            jne  deopt;
            sarq rsi, 1;
            sarq rdx, 1;
        reload:
            // rax = len, rcx = data ptr (inline vs heap storage select)
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            // Shared (copy-on-write) string: the buffer is aliased by
            // other sharers and must not be written in place — detach it
            // (out of line below) and retry.
            // (rcx is free here until the `lea` below, so use it for the
            // tag scratch rather than r8 — r8-r11 are the allocatable pool.)
            movq rcx, (crate::rvalue::STRING_SHARED_TAG);
            cmpq rax, rcx;
            jeq  shared;
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
            jmp  done;
        shared:
        }
        // Out-of-line detach-and-retry: `str_detach` copies the viewed
        // bytes into a fresh owned buffer (plain malloc, no Value
        // allocation, no GC), after which the reload takes the owned path.
        // rdi (recv) / rsi (untagged idx) / rdx (untagged byte) survive in
        // the save area.
        self.jit.save_registers();
        monoasm! { &mut self.jit,
            movq rax, (crate::codegen::runtime::str_detach as *const u8);
            call rax;
        }
        self.jit.restore_registers();
        monoasm! { &mut self.jit,
            jmp  reload;
        done:
        }
    }

    /// `String#<<` with the two hot argument shapes appended in line,
    /// selected per call site by `hint` (`string_shl_gen`'s proof of the
    /// argument class; `Both` emits both paths with the Fixnum tag test
    /// dispatching).
    ///
    /// **Fixnum byte**: a byte 0..=127 appends into any encoding (it is its
    /// own one-byte encode in every ASCII-compatible receiver and the
    /// raw-byte append everywhere else — exactly what the builtin does); a
    /// byte with the high bit set only into ASCII-8BIT (other encodings
    /// multi-byte-encode or reject it). Keeps the cached code-range
    /// classification consistent with
    /// `RStringInner::extend_from_slice_checked`: an ASCII byte preserves
    /// the cache, a high byte degrades it to Unknown.
    ///
    /// **String argument** (compatible encoding + capacity available): a
    /// heap String argument whose encoding tag equals the receiver's —
    /// restricted to the payload-free `Encoding` discriminants 0..=6
    /// (Ascii8..Utf32Be), where one byte is the whole value — or whose
    /// mismatched pairing `Encoding.compatible?` resolves to the receiver's
    /// encoding anyway (both ASCII-compatible, the piece cached SevenBit,
    /// the receiver cached SevenBit/Valid — the erubi buffer shape) is
    /// byte-copied straight into the receiver's spare capacity, and the
    /// cached code range is folded exactly as `RStringInner::extend` does
    /// (SevenBit+SevenBit keeps SevenBit, both ∈ {SevenBit, Valid} →
    /// Valid, anything else → Unknown). A shared (copy-on-write) receiver
    /// is detached in place by
    /// `detach` = `str_detach(recv)` (infallible, GC-free) and the append
    /// retried — sharing is a per-`dup` event, not a reason to leave the
    /// fast path for good. The *argument* may be shared: its `ptr`/`len`
    /// overlay the spilled SmallVec fields, so the heap-side loads read it
    /// correctly.
    ///
    /// Everything else — frozen/chilled receiver, non-Fixnum/non-String
    /// argument, byte out of range, encoding mismatch or a payload-carrying
    /// encoding, insufficient capacity (growth is a normal recurring event,
    /// so it must *not* deopt) — falls back to `f` = `string_shl(vm,
    /// globals, recv, arg)`, the builtin's full semantics (encoding
    /// negotiation, `#to_str` coercion, warn-and-unchill, errors via the
    /// trailing HandleError).
    ///
    /// ### in
    /// - rdi: receiver: String (class-guarded)
    /// - rsi: argument: Value (unguarded)
    ///
    /// ### out
    /// - rax: the receiver (`<<` returns self) / the helper's result
    ///
    /// ### destroy
    /// - rax, rcx, rdx, r8-r11 (+ caller-saved on the fallback call; the
    ///   GP pool was flushed by the generator's `get_using_fpr`)
    pub(crate) fn emit_string_shl(&mut self, f: u64, detach: u64, hint: StringShlHint) {
        let fallback = self.jit.label();
        let exit = self.jit.label();
        let str_entry = self.jit.label();
        // where the Fixnum tag test goes when the argument is not a Fixnum
        let not_fixnum = if hint == StringShlHint::Both {
            str_entry.clone()
        } else {
            fallback.clone()
        };
        if hint != StringShlHint::Str {
            let enc_ok = self.jit.label();
            let heap = self.jit.label();
            let stored = self.jit.label();
            let keep_cr = self.jit.label();
            monoasm! { &mut self.jit,
                // only a Fixnum byte inlines here
                testq rsi, 1;
                jz   not_fixnum;
                // frozen (0b010) or chilled (0b100) → helper raises / warns
                testb [rdi + (RVALUE_OFFSET_FLAG)], (0b110);
                jnz  fallback;
                movq rdx, rsi;
                sarq rdx, 1;
                // byte range 0..=255 (unsigned compare catches negatives)
                cmpq rdx, (0xff);
                ja   fallback;
                // A raw single-byte append only answers where a 7-bit
                // codepoint *is* its byte: ASCII-8BIT / UTF-8 /
                // US-ASCII. In UTF-16 and UTF-32 it is two or four
                // bytes, and the multibyte encodings past US-ASCII
                // build sequences the fast path does not know.
                cmpb [rdi + (crate::rvalue::STRING_TY_OFFSET)], (crate::rvalue::STRING_TY_MAX_INLINE_SHL);
                ja   fallback;
                cmpq rdx, (0x7f);
                jle  enc_ok;
                // high byte: raw append only into ASCII-8BIT (Ascii8 == 0)
                cmpb [rdi + (crate::rvalue::STRING_TY_OFFSET)], (0);
                jne  fallback;
            enc_ok:
                movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
                // shared (copy-on-write) buffer → helper detaches
                movq rcx, (crate::rvalue::STRING_SHARED_TAG);
                cmpq rax, rcx;
                jeq  fallback;
                cmpq rax, (STRING_INLINE_CAP);
                jgt  heap;
                // inline buffer: rax is the length, STRING_INLINE_CAP the
                // capacity; the `cmpq` above already set the full-buffer flags.
                jeq  fallback;
                lea  rcx, [rdi + (RVALUE_OFFSET_INLINE)];
                movb [rcx + rax], rdx;
                addq [rdi + (RVALUE_OFFSET_ARY_CAPA)], 1;
                jmp  stored;
            heap:
                // spilled buffer: rax is the capacity, the length lives beside
                // the pointer. A full buffer reallocates via the helper.
                movq rcx, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
                cmpq rcx, rax;
                jge  fallback;
                movq rax, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
                movb [rax + rcx], rdx;
                addq [rdi + (RVALUE_OFFSET_HEAP_LEN)], 1;
            stored:
                cmpq rdx, (0x7f);
                jle  keep_cr;
                movb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::Unknown as u64);
            keep_cr:
                movq rax, rdi;
                jmp  exit;
            }
        }
        let retry = self.jit.label();
        let detach_path = self.jit.label();
        if hint != StringShlHint::Fixnum {
            let enc_mixed = self.jit.label();
            let recv_ready = self.jit.label();
            let arg_ready = self.jit.label();
            let copy_loop = self.jit.label();
            let copy_done = self.jit.label();
            let set_valid = self.jit.label();
            let set_unknown = self.jit.label();
            self.jit.bind_label(str_entry);
            monoasm! { &mut self.jit,
                // only a heap String argument inlines here
                testq rsi, (0b111);
                jnz  fallback;
                cmpb [rsi + (RVALUE_OFFSET_TY)], (ObjTy::STRING.get());
                jne  fallback;
                // frozen (0b010) or chilled (0b100) → helper raises / warns
                testb [rdi + (RVALUE_OFFSET_FLAG)], (0b110);
                jnz  fallback;
                // Payload-free encodings only: discriminants 0..=6
                // (Ascii8..Utf32Be) carry no payload byte, so one byte is
                // the whole `Encoding`; payload-carrying / exotic
                // encodings (Iso8859(n), Sjis(n), …) go to the helper's
                // full negotiation. Equal encodings pass outright; a
                // *mismatched* pair still appends in place when both are
                // ASCII-compatible (0..=2: Ascii8/Utf8/UsAscii), the
                // piece is cached SevenBit, and the receiver is cached
                // SevenBit or Valid — `Encoding.compatible?` then answers
                // the receiver's encoding (rule 1 when the receiver is
                // 7-bit, rule 2 when only the piece is), so the tag stays
                // put and the fold below is unchanged. Everything else —
                // including a piece whose cr is merely *uncached* — falls
                // back; the helper classifies and caches the piece's cr,
                // so a repeated piece inlines from its second append on.
                movzxb rax, [rdi + (crate::rvalue::STRING_TY_OFFSET)];
                movzxb rcx, [rsi + (crate::rvalue::STRING_TY_OFFSET)];
                cmpq rax, rcx;
                jne  enc_mixed;
                cmpq rax, (6);
                jgt  fallback;
                jmp  retry;
            enc_mixed:
                cmpq rax, (2);
                jgt  fallback;
                cmpq rcx, (2);
                jgt  fallback;
                cmpb [rsi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::SevenBit as u64);
                jne  fallback;
                movzxb rdx, [rdi + (crate::rvalue::STRING_CR_OFFSET)];
                subq rdx, 1;    // SevenBit→0, Valid→1; Unknown wraps, Broken→2
                cmpq rdx, 1;
                ja   fallback;
            retry:
                movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
                // shared (copy-on-write) receiver: detach out of line, retry
                movq rcx, (crate::rvalue::STRING_SHARED_TAG);
                cmpq rax, rcx;
                jeq  detach_path;
                // rcx = recv len, rdx = recv capacity, r8 = recv data,
                // r9 = recv length-slot address (inline vs heap select)
                movq rcx, rax;
                movq rdx, (STRING_INLINE_CAP);
                lea  r8, [rdi + (RVALUE_OFFSET_INLINE)];
                lea  r9, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
                cmpq rax, (STRING_INLINE_CAP);
                jle  recv_ready;
                movq rdx, rax;
                movq rcx, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
                movq r8, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
                lea  r9, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            recv_ready:
                // r10 = arg len, r11 = arg data. A *shared* argument reads
                // fine: its tag (isize::MAX) routes it onto the heap side,
                // where SharedContent's ptr/len overlay the spilled fields.
                movq rax, [rsi + (RVALUE_OFFSET_ARY_CAPA)];
                movq r10, rax;
                lea  r11, [rsi + (RVALUE_OFFSET_INLINE)];
                cmpq rax, (STRING_INLINE_CAP);
                jle  arg_ready;
                movq r10, [rsi + (RVALUE_OFFSET_HEAP_LEN)];
                movq r11, [rsi + (RVALUE_OFFSET_HEAP_PTR)];
            arg_ready:
                // capacity check: growth is the helper's job
                movq rax, rcx;
                addq rax, r10;
                cmpq rax, rdx;
                jgt  fallback;
                // copy arg's bytes to recv_data + recv_len. Even for
                // `s << s` the ranges cannot overlap: the source is
                // [0, len) and the destination [len, 2*len).
                addq r8, rcx;
                xorq rcx, rcx;
            copy_loop:
                cmpq rcx, r10;
                jge  copy_done;
                movzxb rdx, [r11 + rcx];
                movb [r8 + rcx], rdx;
                addq rcx, 1;
                jmp  copy_loop;
            copy_done:
                // bump the stored length (the capa slot doubles as the
                // length while inline; r9 points at the right slot)
                movq [r9], rax;
                // code-range fold, exactly `RStringInner::extend`:
                // SevenBit+SevenBit keeps SevenBit, both {SevenBit, Valid}
                // → Valid, anything else (Unknown/Broken either side) →
                // Unknown.
                movzxb rcx, [rdi + (crate::rvalue::STRING_CR_OFFSET)];
                movzxb rdx, [rsi + (crate::rvalue::STRING_CR_OFFSET)];
                subq rcx, 1;    // SevenBit→0, Valid→1; Unknown wraps, Broken→2
                cmpq rcx, 1;
                ja   set_unknown;
                subq rdx, 1;
                cmpq rdx, 1;
                ja   set_unknown;
                orq  rcx, rdx;
                jnz  set_valid;
                // both SevenBit: the cache is already correct
                movq rax, rdi;
                jmp  exit;
            set_valid:
                movb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::Valid as u64);
                movq rax, rdi;
                jmp  exit;
            set_unknown:
                movb [rdi + (crate::rvalue::STRING_CR_OFFSET)], (CodeRange::Unknown as u64);
                movq rax, rdi;
                jmp  exit;
            }
        }
        self.jit.select_page(1);
        if hint != StringShlHint::Fixnum {
            monoasm! { &mut self.jit,
            detach_path:
                // str_detach(recv): copy the shared view into an owned
                // buffer in place. Infallible, GC-free, answers nothing;
                // recv/arg are caller-saved, so keep them across the call.
                pushq rdi;
                pushq rsi;
                movq rax, (detach);
                call rax;
                popq rsi;
                popq rdi;
                jmp  retry;
            }
        }
        monoasm! { &mut self.jit,
        fallback:
            movq rdx, rdi;
            movq rcx, rsi;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (f);
            call rax;
            jmp  exit;
        }
        self.jit.select_page(0);
        self.jit.bind_label(exit);
    }

    /// Direct call of a two-value runtime helper `f(vm, globals, a, b)`,
    /// skipping the Ruby method frame: `a` in rdx, `b` in rcx. Result Value
    /// in rax (errors via the trailing HandleError). Used by the `Hash#[]`
    /// and `String#<<` inliners.
    pub(crate) fn emit_call_2args(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (f);
            call rax;
        }
    }

    /// `Hash#[]=`: `hashindex_assign(vm, globals, recv, key, val)`. recv in
    /// rdi, key in rsi, value in rdx; the assigned value (what `Hash#[]=`
    /// answers) in rax, errors via the trailing HandleError. The operands are
    /// shuffled up into the C-ABI arg registers back-to-front so no scratch is
    /// needed.
    pub(crate) fn emit_hash_index_assign(&mut self, f: u64) {
        monoasm! { &mut self.jit,
            movq r8, rdx;               // val -> arg4
            movq rcx, rsi;              // key -> arg3
            movq rdx, rdi;              // recv -> arg2
            movq rdi, rbx;              // vm
            movq rsi, r12;              // globals
            movq rax, (f);
            call rax;
        }
    }

    /// `Hash#default=`: hash in rdi, new default in rsi, result (the assigned
    /// value — what `Hash#default=` returns) in rax.
    ///
    /// In-line shapes: a boxed hash that already carries a default box gets
    /// its discriminant/payload overwritten in place (replacing a default
    /// proc exactly as `Hash#default=` does) plus the write barrier; a nil
    /// assignment with no default box is a no-op (no default and a nil
    /// default are the same observable state). Everything else — a first
    /// non-nil default (box allocation, possibly inline→boxed promotion) —
    /// calls `f` = `hash_default_assign_extern(vm, globals, recv, val)`,
    /// whose error edge is the caller's trailing HandleError. The receiver's
    /// un-frozen-ness is already deopt-guarded by the caller.
    pub(crate) fn emit_hash_default_assign(&mut self, f: u64) {
        let no_box = self.jit.label();
        let slow = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let slot = HASH_DEFAULT_OFFSET;
        let payload = HASH_DEFAULT_PAYLOAD_OFFSET;
        let tag_value = HASH_DEFAULT_TAG_VALUE as u64;
        monoasm! { &mut self.jit,
            movl rax, [rdi + (ty_flags)];
            andl rax, (mask);
            cmpl rax, (boxed_rep);
            jne  no_box;                    // inline: never carries a default
            movq rax, [rdi + (slot)];       // Option<Box<HashDefault>>: null = None
            testq rax, rax;
            jeq  no_box;
            movq [rax], (tag_value);        // discriminant := Value
            movq [rax + (payload)], rsi;
        }
        // Write barrier: rdi = the hash (parent), rsi = the stored default.
        self.emit_write_barrier_rdi(GP::Rsi);
        monoasm! { &mut self.jit,
            movq rax, rsi;
            jmp  exit;
        no_box:
            cmpq rsi, (NIL_VALUE);
            jne  slow;
            movq rax, (NIL_VALUE);
        exit:
        }
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        slow:
            movq rdx, rdi;                  // recv -> arg2
            movq rcx, rsi;                  // val -> arg3
            movq rdi, rbx;                  // vm
            movq rsi, r12;                  // globals
            movq rax, (f);
            call rax;
            jmp  exit;
        }
        self.jit.select_page(0);
    }

    /// `Hash#size`: entry count of the hash in `base`, fixnum-tagged into `dst`.
    ///
    /// A small hash keeps its length in the header's representation bits; a
    /// boxed one keeps it in the entry vector. The boxed length hangs off a
    /// pointer that is only a pointer on that side of the branch, so unlike
    /// `Array#size` this cannot be a speculative load plus `cmov`.
    /// `sub_dead` additionally subtracts the boxed tombstone count
    /// (`HASH_DEAD_OFFSET`) — the live size for `Hash#size`. Without it the
    /// raw entry-vector length is produced (`__entry_count`), the exclusive
    /// bound for a position-indexed walk. Destroys rsi when `sub_dead`.
    pub(crate) fn gen_hash_len_fixnum(
        &mut self,
        dst: GP,
        base: GP,
        layout: rubymap::EntriesLayout,
        sub_dead: bool,
    ) {
        let (d, b) = (dst as u64, base as u64);
        let tag = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let len_off = layout.len_offset;
        let dead_off = HASH_DEAD_OFFSET;
        monoasm! { &mut self.jit,
            // The representation bits live in the low byte of ty_flags; the
            // 32-bit load stays inside the header and `andl` clears the rest.
            movl R(d), [R(b) + (ty_flags)];
            andl R(d), (mask);
            cmpl R(d), (boxed_rep);
            jne  tag;
        }
        if sub_dead {
            monoasm! { &mut self.jit,
                movl rsi, [R(b) + (dead_off)];
            }
        }
        monoasm! { &mut self.jit,
            movq R(d), [R(b) + (map_ptr)];
            movq R(d), [R(d) + (len_off)];
        }
        if sub_dead {
            monoasm! { &mut self.jit,
                subq R(d), rsi;
            }
        }
        monoasm! { &mut self.jit,
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
        let key_field = layout.key_offset;
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
            // A tombstoned entry answers nil like an out-of-range index —
            // user code may probe any position directly. The boxed maps are
            // keyed by `Option<Value>`, whose `None` sits in `Value`'s
            // `NonZeroU64` niche: a dead key is the all-zero word.
            movq rsi, [rcx + (key_field)];
            testq rsi, rsi;
            jeq  exit;
            movq rax, [rcx + (bucket_field)];
        exit:
        }
    }

    ///
    /// `Hash#[]` with the probe in line — see `AsmInst::HashProbe`.
    ///
    /// ### in
    /// - rdx: the hash
    /// - rcx: the key (class-guarded by the caller to what `digest` /
    ///   `key_eq` expect)
    ///
    /// ### out
    /// - rax: the value on a hit; `hashindex`'s answer on a miss
    ///
    /// rdx and rcx survive the probe (the miss path needs them as
    /// `hashindex`'s arguments); everything else caller-saved is scratch.
    /// Total: a shape the probe does not handle is answered by the builtin,
    /// never by an exit.
    ///
    /// ### The indexed regime
    ///
    /// Past the linear size the map is probed through its hashbrown
    /// indices table, mirroring `RawTableInner::find_inner` without SIMD:
    /// a 16-byte control group is two 64-bit words, each matched against
    /// the digest's 7-bit tag by the SWAR zero-byte test hashbrown's own
    /// generic backend uses (`(x - 0x01..) & !x & 0x80..`; false positives
    /// are possible past a true match and are harmless — every candidate is
    /// verified by its full stored digest). Candidates are read from the
    /// bucket array below the control bytes (bucket `i` at `ctrl - 8(i+1)`)
    /// as indices into the entries. After both words, an EMPTY control byte
    /// (`0xFF`: bits 7 and 6 set, so `w & (w << 1) & 0x80..`) anywhere in
    /// the group ends the walk; otherwise the triangular sequence steps on.
    ///
    pub(crate) fn gen_hash_probe(
        &mut self,
        layout: rubymap::EntriesLayout,
        hashindex: u64,
        digest: u64,
        key_eq: Option<u64>,
    ) {
        let lp = self.jit.label();
        let next = self.jit.label();
        let indexed = self.jit.label();
        let group = self.jit.label();
        let exhausted = self.jit.label();
        let miss = self.jit.label();
        let done = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let default_slot = HASH_DEFAULT_OFFSET;
        let linear_off = layout.linear_offset;
        let len_off = layout.len_offset;
        let ptr_off = layout.ptr_offset;
        let bucket_size = layout.bucket_size;
        let hash_off = layout.hash_offset;
        let ctrl_off = layout.indices_ctrl_offset;
        let mask_off = layout.indices_mask_offset;
        let content = HASH_CONTENT_OFFSET;
        monoasm! { &mut self.jit,
            // The boxed, value-keyed representation only: inline pairs have
            // no digests to compare, and the identity-keyed map digests the
            // key's identity, not its content (a String key's digest would
            // not even find its own object).
            movzxb rsi, [rdx + (ty_flags)];
            andq rsi, (mask);
            cmpq rsi, (boxed_rep);
            jne  miss;
            movq rsi, [rdx + (content)];        // 0 = Map, 1 = IdentMap
            testq rsi, rsi;
            jne  miss;
            // digest = digest(key). A leaf, but a C call: rdx / rcx are
            // caller-saved, and the miss path still needs them.
            pushq rdx;
            pushq rcx;
            movq rdi, rcx;
            movq rax, (digest);
            call rax;
            popq rcx;
            popq rdx;
            movq r8, rax;                       // r8 = digest
            movq rdi, [rdx + (map_ptr)];
            movzxb rsi, [rdi + (linear_off)];
            testq rsi, rsi;
            jz   indexed;
            // Linear regime (no indices table): the entries are the whole
            // probe.
            movq r11, [rdi + (ptr_off)];        // r11 = &entries[0]
            movq r9, [rdi + (len_off)];
            movq rsi, (bucket_size);
            imul r9, rsi;
            addq r9, r11;                       // r9 = one past the last entry
        lp:
            cmpq r11, r9;
            jae  exhausted;
            cmpq r8, [r11 + (hash_off)];
            jne  next;
        }
        // entry in r11; rsi / rdi / r10 are free here.
        self.gen_hash_probe_key_check(&layout, key_eq, 11, 10, &next, &miss, &done);
        monoasm! { &mut self.jit,
        next:
            addq r11, (bucket_size);
            jmp  lp;
        indexed:
        }
        if layout.group_width == 16 {
            let lo = 0x0101_0101_0101_0101u64;
            let hi = 0x8080_8080_8080_8080u64;
            monoasm! { &mut self.jit,
                movq rsi, r8;
                andq rsi, [rdi + (mask_off)];   // rsi = pos = h1 & bucket_mask
                xorq rdi, rdi;                  // rdi = stride
            group:
            }
            // The two words of the group at pos; rsi walks pos, pos + 8.
            for word in 0..2 {
                if word == 1 {
                    monoasm! { &mut self.jit, addq rsi, 8; }
                }
                self.gen_hash_probe_word(&layout, key_eq, lo, hi, &miss, &done);
            }
            monoasm! { &mut self.jit,
                // Back to pos. An EMPTY byte in the group ends the walk.
                subq rsi, 8;
                movq r9, [rdx + (map_ptr)];
                movq r9, [r9 + (ctrl_off)];
                movq rax, [r9 + rsi];
                movq r10, rax;
                shlq r10, 1;
                andq rax, r10;
                movq r11, [r9 + rsi + 8];
                movq r10, r11;
                shlq r10, 1;
                andq r11, r10;
                orq  rax, r11;
                movq r10, (hi);
                testq rax, r10;
                jne  exhausted;
                // pos = (pos + stride) & bucket_mask, stride += 16.
                addq rdi, 16;
                addq rsi, rdi;
                movq r11, [rdx + (map_ptr)];
                andq rsi, [r11 + (mask_off)];
                jmp  group;
            }
        } else {
            // A control group this emitter does not know how to scan: the
            // builtin walks the table.
            monoasm! { &mut self.jit, jmp miss; }
        }
        monoasm! { &mut self.jit,
        exhausted:
            // Not present. Without a default that *is* the answer, and the
            // builtin would only redo the digest and the probe to say so;
            // `Option<Box<HashDefault>>` is null when there is neither a
            // default value nor a default proc.
            movq rsi, [rdx + (default_slot)];
            testq rsi, rsi;
            jne  miss;
            movq rax, (NIL_VALUE);
            jmp  done;
        miss:
        }
        // A default value / default proc, or a shape this probe does not
        // handle: the builtin.
        self.emit_call_2args(hashindex);
        self.jit.bind_label(done);
    }

    /// One control word of the indexed probe (`gen_hash_probe`): rsi is
    /// the word's position in the control bytes, r8 the digest; rax / r9 /
    /// r10 / r11 are scratch. Every tag match is looked up through the
    /// bucket array and verified against the entry's stored digest, then
    /// its key. Falls through when the word is done.
    fn gen_hash_probe_word(
        &mut self,
        layout: &rubymap::EntriesLayout,
        key_eq: Option<u64>,
        lo: u64,
        hi: u64,
        miss: &DestLabel,
        done: &DestLabel,
    ) {
        let cand = self.jit.label();
        let cand_next = self.jit.label();
        let word_done = self.jit.label();
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let ptr_off = layout.ptr_offset;
        let bucket_size = layout.bucket_size;
        let hash_off = layout.hash_offset;
        let ctrl_off = layout.indices_ctrl_offset;
        let mask_off = layout.indices_mask_offset;
        monoasm! { &mut self.jit,
            movq r9, [rdx + (map_ptr)];
            movq r9, [r9 + (ctrl_off)];
            movq rax, [r9 + rsi];               // the control word
            movq r9, r8;
            shrq r9, 57;                        // the digest's 7-bit tag
            movq r10, (lo);
            imul r9, r10;                       // ... in every byte
            xorq rax, r9;                       // x: a zero byte per match
            movq r9, rax;
            notq r9;
            subq rax, r10;
            andq rax, r9;
            movq r10, (hi);
            andq rax, r10;                      // rax = match mask (bit 7 of each byte)
            movq r9, rsi;                       // r9 = position of the byte at bit 0
        cand:
            testq rax, rax;
            jz   word_done;
            testq rax, (0x80);
            jz   cand_next;
            // Candidate: bucket (r9 & mask) holds an index into the entries.
            movq r10, [rdx + (map_ptr)];
            movq r11, r9;
            andq r11, [r10 + (mask_off)];
            notq r11;                           // -(index + 1)
            movq r10, [r10 + (ctrl_off)];
            movq r10, [r10 + r11 * 8];          // the entry's index
            movq r11, (bucket_size);
            imul r10, r11;
            movq r11, [rdx + (map_ptr)];
            addq r10, [r11 + (ptr_off)];        // r10 = the entry
            cmpq r8, [r10 + (hash_off)];
            jne  cand_next;
        }
        // entry in r10; r11 is free (rax / r9 are the loop state).
        self.gen_hash_probe_key_check(layout, key_eq, 10, 11, &cand_next, miss, done);
        monoasm! { &mut self.jit,
        cand_next:
            shrq rax, 8;
            addq r9, 1;
            jmp  cand;
        word_done:
        }
    }

    /// The key comparison of `gen_hash_probe` for the entry in `R(entry)`
    /// whose stored digest matched: a hit loads the value into rax and jumps
    /// to `done`; a stored key that is not this key continues at `next`.
    /// With `key_eq`, identity decides first, then the leaf; a leaf verdict
    /// of "not equal" against a full-digest match is (short of a 64-bit
    /// collision) a tombstone or a foreign key, and goes to `miss` — the
    /// builtin answers, and nothing of the probe's state need survive the
    /// call. `R(tmp)` is scratch.
    fn gen_hash_probe_key_check(
        &mut self,
        layout: &rubymap::EntriesLayout,
        key_eq: Option<u64>,
        entry: u64,
        tmp: u64,
        next: &DestLabel,
        miss: &DestLabel,
        done: &DestLabel,
    ) {
        let key_off = layout.key_offset;
        let value_off = layout.value_offset;
        match key_eq {
            None => {
                monoasm! { &mut self.jit,
                    // `Option<Value>` in `Value`'s NonZero niche: `Some(k)` is
                    // k's own bits, and a tombstone's `None` is the zero word,
                    // which no packed key equals.
                    cmpq rcx, [R(entry) + (key_off)];
                    jne  next;
                    movq rax, [R(entry) + (value_off)];
                    jmp  done;
                }
            }
            Some(key_eq) => {
                let hit = self.jit.label();
                monoasm! { &mut self.jit,
                    movq R(tmp), [R(entry) + (key_off)];
                    testq R(tmp), R(tmp);           // a tombstone
                    jz   next;
                    cmpq R(tmp), rcx;               // the stored object itself
                    jeq  hit;
                    pushq rdx;
                    pushq rcx;
                    pushq R(entry);
                    subq rsp, 8;                    // keep the call 16-aligned
                    movq rdi, R(tmp);
                    movq rsi, rcx;
                    movq rax, (key_eq);
                    call rax;
                    addq rsp, 8;
                    popq R(entry);
                    popq rcx;
                    popq rdx;
                    testq rax, rax;
                    jz   miss;
                hit:
                    movq rax, [R(entry) + (value_off)];
                    jmp  done;
                }
            }
        }
    }

    /// `Hash#__live_at`: hash in rdx, fixnum index in rcx, Ruby bool in rax —
    /// `true` iff the position is in range and the entry is not a tombstone.
    /// Total by construction, like `__key_at`. rsi and rdi are scratch.
    pub(crate) fn gen_hash_live_at(&mut self, layout: rubymap::EntriesLayout) {
        let boxed = self.jit.label();
        let dead = self.jit.label();
        let live = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = RVALUE_OFFSET_TY + 1;
        let mask = HASH_REP_MASK as u64;
        let boxed_rep = HASH_REP_BOXED as u64;
        let map_ptr = HASH_CONTENT_MAP_OFFSET;
        let len_off = layout.len_offset;
        let ptr_off = layout.ptr_offset;
        let bucket_size = layout.bucket_size;
        let key_off = layout.key_offset;
        monoasm! { &mut self.jit,
            sarq rcx, 1;                  // untag the index
            js   dead;                    // negative → false
            movl rsi, [rdx + (ty_flags)];
            andl rsi, (mask);
            cmpl rsi, (boxed_rep);
            jeq  boxed;
            // Inline: the representation bits double as the length, and an
            // inline hash never holds a tombstone — in range means live.
            cmpq rcx, rsi;
            jb   live;
            jmp  dead;
        boxed:
            movq rdi, [rdx + (map_ptr)];
            cmpq rcx, [rdi + (len_off)];
            jae  dead;
            movq rsi, (bucket_size);
            imul rcx, rsi;
            addq rcx, [rdi + (ptr_off)];
            movq rsi, [rcx + (key_off)];
            testq rsi, rsi;                // None (dead) is the zero word
            jeq  dead;
        live:
            movq rax, (TRUE_VALUE);
            jmp  exit;
        dead:
            movq rax, (FALSE_VALUE);
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
