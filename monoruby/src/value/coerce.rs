use super::*;
use num::{BigInt, FromPrimitive, ToPrimitive};

/// CRuby's `rb_check_convert_type_with_id(recv, _, tname, method)`: the
/// result of the implicit conversion `method`, or `None` when `recv`
/// does not convert — it does not answer `method` (asked through
/// [`check_funcall`], so `method_missing` counts), or answers nil. An
/// answer that `is` rejects is the conversion mismatch.
pub(crate) fn check_convert_type(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    method: IdentId,
    tname: &str,
    is: fn(Value) -> bool,
) -> Result<Option<Value>> {
    match check_funcall(vm, globals, recv, method)? {
        Some(v) if is(v) => Ok(Some(v)),
        Some(v) if !v.is_nil() => Err(MonorubyErr::cant_convert_error(
            globals, recv, v, tname, method,
        )),
        _ => Ok(None),
    }
}

/// CRuby's `rb_convert_type_with_id`: as [`check_convert_type`], but
/// the conversion is required — `None` means `recv` does not answer
/// `method` (the caller words "no implicit conversion"), and a nil
/// answer is a mismatch like any other.
pub(crate) fn convert_type(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    method: IdentId,
    tname: &str,
    is: fn(Value) -> bool,
) -> Result<Option<Value>> {
    match check_funcall(vm, globals, recv, method)? {
        Some(v) if is(v) => Ok(Some(v)),
        Some(v) => Err(MonorubyErr::cant_convert_error(
            globals, recv, v, tname, method,
        )),
        None => Ok(None),
    }
}

fn is_string(v: Value) -> bool {
    v.is_rstring().is_some()
}

fn is_array(v: Value) -> bool {
    v.try_array_ty().is_some()
}

fn is_hash(v: Value) -> bool {
    v.try_hash_ty().is_some()
}

fn coerce_to_rstring_inner(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    methods: &[IdentId],
) -> Result<RString> {
    if let Some(s) = recv.is_rstring() {
        return Ok(s);
    }
    for &method in methods {
        if method == IdentId::TO_PATH {
            // CRuby's `rb_get_path`: `to_path` is asked first and what
            // it answers is then a String by `StringValue` (`to_str`).
            if let Some(path) = check_funcall(vm, globals, recv, method)? {
                return path.coerce_to_rstring(vm, globals);
            }
        } else if let Some(s) = convert_type(vm, globals, recv, method, "String", is_string)? {
            return Ok(s.is_rstring().unwrap());
        }
    }
    Err(MonorubyErr::no_implicit_conversion(
        globals,
        recv,
        STRING_CLASS,
    ))
}

impl Value {
    pub(crate) fn coerce_to_array(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Array> {
        if let Some(ary) = self.try_array_ty() {
            return Ok(ary);
        }
        match convert_type(vm, globals, *self, IdentId::TO_ARY, "Array", is_array)? {
            Some(ary) => Ok(ary.try_array_ty().unwrap()),
            None => Err(MonorubyErr::no_implicit_conversion(
                globals,
                *self,
                ARRAY_CLASS,
            )),
        }
    }

    /// CRuby's `rb_check_array_type`: the Array `self` is or converts to
    /// by `to_ary`, `None` when it does not convert.
    pub(crate) fn check_array_type(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Array>> {
        if let Some(ary) = self.try_array_ty() {
            return Ok(Some(ary));
        }
        Ok(
            check_convert_type(vm, globals, *self, IdentId::TO_ARY, "Array", is_array)?
                .map(|v| v.try_array_ty().unwrap()),
        )
    }

    /// CRuby's `rb_check_string_type`: the String `self` is or converts
    /// to by `to_str`, `None` when it does not convert.
    pub(crate) fn check_string_type(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        if self.is_rstring().is_some() {
            return Ok(Some(*self));
        }
        check_convert_type(vm, globals, *self, IdentId::TO_STR, "String", is_string)
    }

    /// CRuby's `rb_check_hash_type`: the Hash `self` is or converts to
    /// by `to_hash`, `None` when it does not convert.
    pub(crate) fn check_hash_type(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Hashmap>> {
        if let Some(h) = self.try_hash_ty() {
            return Ok(Some(h));
        }
        Ok(check_convert_type(
            vm,
            globals,
            *self,
            IdentId::get_id("to_hash"),
            "Hash",
            is_hash,
        )?
        .map(|v| v.try_hash_ty().unwrap()))
    }

    /// CRuby's `rb_check_to_int`: the Integer `self` is or answers from
    /// `to_int`; `None` when it has no `to_int` or answers something
    /// else (which is not an error here).
    pub(crate) fn check_to_int(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<Value>> {
        if self.is_integer() {
            return Ok(Some(*self));
        }
        Ok(check_funcall(vm, globals, *self, IdentId::TO_INT)?.filter(|v| v.is_integer()))
    }

    ///
    /// Try to convert `self` to i64.
    ///
    /// - if `self` is a Fixnum, return it as i64.
    /// - if `self` is a Float, return it as i64.
    /// - if `self` is a Bignum, return RangeError.
    ///
    pub fn coerce_to_i64(&self, store: &Store) -> Result<i64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i),
            RV::Float(f) => {
                if f.is_nan() || f.is_infinite() || f < (i64::MIN as f64) || (i64::MAX as f64) < f {
                    Err(MonorubyErr::float_out_of_range_of_integer(f))
                } else {
                    Ok(f.trunc() as i64)
                }
            }
            RV::BigInt(_) => Err(MonorubyErr::rangeerr(
                "bignum too big to convert into 'long'",
            )),
            _ => Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                INTEGER_CLASS,
            )),
        }
    }

    ///
    /// Try conversion from `self` to i64 using `to_int`.
    ///
    /// - if `self` is a Fixnum, return it as i64.
    /// - if `self` is a Bignum, return it as i64 if it fits, otherwise return RangeError.
    /// - if `self` responds to `to_int`, call it and try conversion on the result.
    /// - otherwise, return TypeError.
    ///
    #[inline]
    pub(crate) fn coerce_to_int_i64(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<i64> {
        if let Some(i) = self.try_fixnum() {
            return Ok(i);
        }
        self.coerce_to_int_i64_slow(vm, globals)
    }

    ///
    /// CRuby's `NUM2LONG`, which is how a C method reads a count or a
    /// size: a Float truncates without a call (one outside the range of
    /// a `long` is a RangeError naming it), `nil` has a message of its
    /// own, and anything else goes through `to_int`
    /// ([`Self::coerce_to_int_i64`]). `builtins/kernel.rb` has the same
    /// as `__num2long`.
    ///
    #[inline]
    pub(crate) fn num2long(&self, vm: &mut Executor, globals: &mut Globals) -> Result<i64> {
        if let Some(i) = self.try_fixnum() {
            return Ok(i);
        }
        self.num2long_slow(vm, globals)
    }

    #[inline(never)]
    fn num2long_slow(&self, vm: &mut Executor, globals: &mut Globals) -> Result<i64> {
        if let Some(f) = self.try_float() {
            // NaN fails both comparisons.
            return if f < 9223372036854775808.0 && f >= -9223372036854775808.0 {
                Ok(f as i64)
            } else {
                Err(MonorubyErr::float_out_of_range_of_integer(f))
            };
        }
        if self.is_nil() {
            return Err(MonorubyErr::typeerr(
                "no implicit conversion from nil to integer",
            ));
        }
        self.coerce_to_int_i64(vm, globals)
    }

    #[inline(never)]
    fn coerce_to_int_i64_slow(&self, vm: &mut Executor, globals: &mut Globals) -> Result<i64> {
        let res = match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => *self,
            _ => self.coerce_to_int(vm, globals)?,
        };
        match res.unpack() {
            RV::Fixnum(i) => return Ok(i),
            RV::BigInt(b) => {
                return b
                    .to_i64()
                    .ok_or_else(|| MonorubyErr::rangeerr("bignum too big to convert into 'long'"));
            }
            _ => unreachable!(),
        };
    }

    /// Coerce to u64 for Array#pack. Extracts the low 64 bits from BigInts
    /// (matching CRuby's truncation semantics) instead of raising RangeError.
    pub(crate) fn coerce_to_pack_u64(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<u64> {
        let res = match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => *self,
            RV::Float(f) => {
                let i = f as i64;
                return Ok(i as u64);
            }
            _ => self.coerce_to_int(vm, globals)?,
        };
        match res.unpack() {
            RV::Fixnum(i) => Ok(i as u64),
            RV::BigInt(b) => {
                // Extract low 64 bits from the BigInt's magnitude,
                // then apply sign (two's complement).
                let (sign, digits) = b.to_u64_digits();
                let abs_low = digits.first().copied().unwrap_or(0);
                Ok(if sign == num::bigint::Sign::Minus {
                    (abs_low as i64).wrapping_neg() as u64
                } else {
                    abs_low
                })
            }
            _ => unreachable!(),
        }
    }

    pub(crate) fn coerce_to_int(&self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        // `rb_to_int` probes with `rb_check_funcall`, so a redefined
        // `respond_to?` is consulted (and can veto) and a
        // `method_missing`-backed `to_int` converts.
        let Some(result) = check_funcall(vm, globals, *self, IdentId::TO_INT)? else {
            return Err(MonorubyErr::no_implicit_conversion(
                globals,
                *self,
                INTEGER_CLASS,
            ));
        };
        match result.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => Ok(result),
            _ => Err(MonorubyErr::cant_convert_error_int(globals, *self, result)),
        }
    }

    /// Convert `self` to Integer (Fixnum or BigInt) for sprintf.
    ///
    /// Tries direct conversion first, then to_int, then to_i as fallback.
    /// Accepts Float (truncated), String (parsed).
    /// Raises TypeError if no conversion is possible.
    pub(crate) fn coerce_to_integer(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<IntegerBase> {
        match self.unpack() {
            RV::Fixnum(i) => return Ok(IntegerBase::Fixnum(i)),
            RV::BigInt(b) => return Ok(IntegerBase::BigInt(b.clone())),
            RV::Float(f) => {
                let t = f.trunc();
                return if i64::MIN as f64 <= t && t <= i64::MAX as f64 {
                    Ok(IntegerBase::Fixnum(t as i64))
                } else {
                    Ok(IntegerBase::BigInt(
                        BigInt::from_f64(t).expect("float is not NaN or infinite"),
                    ))
                };
            }
            RV::String(s) => {
                // CRuby's sprintf integer specifiers (%b/%d/%i/%o/%u/%x/%X)
                // treat a String operand the same as `Kernel#Integer(s)`,
                // so they honour `0x`/`0b`/`0o` prefixes, leading-zero
                // octal, and underscore separators. Routing through the
                // shared parser keeps the behaviour aligned.
                let s = s.check_utf8()?;
                let v = crate::builtins::parse_kernel_integer(s, 0)?;
                return Ok(match v.unpack() {
                    RV::Fixnum(i) => IntegerBase::Fixnum(i),
                    RV::BigInt(b) => IntegerBase::BigInt(b.clone()),
                    _ => unreachable!("parse_kernel_integer returns Fixnum or BigInt"),
                });
            }
            _ => {}
        };
        // Try to_int first, then to_i as fallback (matching CRuby sprintf behavior).
        if let Ok(i) = self.coerce_to_int_i64(vm, globals) {
            return Ok(IntegerBase::Fixnum(i));
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::get_id("to_i")) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            match result.unpack() {
                RV::Fixnum(i) => return Ok(IntegerBase::Fixnum(i)),
                RV::BigInt(b) => return Ok(IntegerBase::BigInt(b.clone())),
                _ => {}
            }
        }
        Err(MonorubyErr::typeerr(format!(
            "can't convert {} into Integer",
            self.builtin_class_name(&globals.store)
        )))
    }

    /// Convert `self` to f64 for sprintf.
    ///
    /// Tries direct conversion first, then to_f.
    /// Accepts Fixnum, Float, String (parsed).
    /// Raises TypeError if no conversion is possible.
    pub(crate) fn coerce_to_float(&self, vm: &mut Executor, globals: &mut Globals) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => return Ok(i as f64),
            RV::Float(f) => return Ok(f),
            RV::String(s) => {
                // Match `Kernel#Float(s)` semantics here so sprintf's
                // float specifiers (%e/%E/%f/%g/%G/%a/%A) accept hex
                // floats and underscore separators consistently.
                let s = s.check_utf8()?;
                let v = crate::builtins::parse_kernel_float(s)?;
                if let RV::Float(f) = v.unpack() {
                    return Ok(f);
                }
                unreachable!("parse_kernel_float returns Float");
            }
            _ => {}
        };
        if let Ok(f) = self.coerce_to_f64(vm, globals) {
            return Ok(f);
        }
        Err(MonorubyErr::cant_convert_into_float(globals, *self))
    }

    ///
    /// Try to convert `self` to i64.
    ///
    /// - if `self` is a Fixnum or a Bignum, convert it to f64.
    /// - if `self` is a Float, return it as f64.
    ///
    /// Convert to f64 from numeric types only (Fixnum/Float/BigInt).
    /// Does NOT call to_f. Use coerce_to_f64() for the full version.
    pub fn coerce_to_f64_no_convert(&self, store: &Store) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i as f64),
            RV::Float(f) => Ok(f),
            RV::BigInt(b) => {
                if let Some(f) = b.to_f64() {
                    Ok(f)
                } else {
                    Err(MonorubyErr::cant_convert_into_float(store, *self))
                }
            }
            _ => Err(MonorubyErr::no_implicit_conversion(
                store,
                *self,
                FLOAT_CLASS,
            )),
        }
    }

    /// Convert to f64, calling `to_f` if the value is not a numeric type.
    /// This is the standard coercion method matching CRuby behavior.
    pub(crate) fn coerce_to_f64(&self, vm: &mut Executor, globals: &mut Globals) -> Result<f64> {
        match self.unpack() {
            RV::Fixnum(i) => Ok(i as f64),
            RV::Float(f) => Ok(f),
            RV::BigInt(b) => {
                if let Some(f) = b.to_f64() {
                    Ok(f)
                } else {
                    Err(MonorubyErr::cant_convert_into_float(&globals.store, *self))
                }
            }
            _ => {
                if self.is_str().is_none()
                    && let Some(result) = check_funcall(vm, globals, *self, IdentId::TO_F)?
                {
                    match result.unpack() {
                        RV::Float(f) => Ok(f),
                        RV::Fixnum(i) => Ok(i as f64),
                        _ => Err(MonorubyErr::cant_convert_error_f(globals, *self, result)),
                    }
                } else {
                    Err(MonorubyErr::no_implicit_conversion(
                        &globals.store,
                        *self,
                        FLOAT_CLASS,
                    ))
                }
            }
        }
    }

    ///
    /// Try to convert `f` to Integer.
    ///
    pub fn coerce_f64_to_int(f: f64) -> Result<Value> {
        if f.is_nan() || f.is_infinite() {
            Err(MonorubyErr::float_out_of_range_of_integer(f))
        } else if (i64::MIN as f64) < f && f < (i64::MAX as f64) {
            Ok(Value::integer(f as i64))
        } else if let Some(b) = BigInt::from_f64(f) {
            Ok(Value::bigint(b))
        } else {
            Err(MonorubyErr::float_out_of_range_of_integer(f))
        }
    }

    /// Coerce the value to a Hashmap by trying to_hash if necessary.
    pub(crate) fn coerce_to_hash(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Hashmap> {
        if let Some(h) = self.try_hash_ty() {
            return Ok(h);
        }
        match convert_type(
            vm,
            globals,
            *self,
            IdentId::get_id("to_hash"),
            "Hash",
            is_hash,
        )? {
            Some(h) => Ok(h.try_hash_ty().unwrap()),
            None => Err(MonorubyErr::no_implicit_conversion(
                &globals.store,
                *self,
                HASH_CLASS,
            )),
        }
    }

    /// Like [`expect_symbol_or_string`], but additionally tries `#to_str`
    /// coercion for non-Symbol/non-String values, matching CRuby's
    /// `rb_check_id` semantics for `attr_*` and similar APIs.
    ///
    /// - Symbol or String: return the corresponding `IdentId` directly.
    /// - Otherwise: invoke `#to_str` on the receiver. If it returns a
    ///   String, intern that. If it returns anything else (or doesn't
    ///   exist), raise TypeError.
    pub(crate) fn coerce_to_symbol_or_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<IdentId> {
        if let Some(id) = self.try_symbol() {
            return Ok(id);
        }
        // A String is interned as `String#to_sym` would (`rb_to_id` →
        // `rb_str_intern`): bytes invalid in its encoding are an
        // EncodingError, not a name.
        if let Some(inner) = self.is_rstring_inner() {
            return crate::builtins::string::intern_name_string(&globals.store, inner);
        }
        // `rb_check_id`: `rb_check_string_type`, so `to_str` is asked
        // through `rb_check_funcall` and a non-String answer is the
        // conversion mismatch.
        if let Some(s) = self.check_string_type(vm, globals)? {
            return crate::builtins::string::intern_name_string(
                &globals.store,
                s.as_rstring_inner(),
            );
        }
        Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, *self))
    }

    /// A method / variable / constant name argument (`rb_check_id`,
    /// `rb_to_id`): a Symbol or a String as
    /// [`Self::expect_symbol_or_string`] reads it, and anything else by
    /// `rb_check_string_type` — `to_str` through `rb_check_funcall`.
    pub(crate) fn expect_symbol_or_to_str(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<IdentId> {
        if self.try_symbol().is_some() || self.is_rstring().is_some() {
            return self.expect_symbol_or_string(&globals.store);
        }
        match self.check_string_type(vm, globals)? {
            Some(s) => s.expect_symbol_or_string(&globals.store),
            None => Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, *self)),
        }
    }

    /// [`Self::expect_symbol_or_to_str`] for a query that must not
    /// intern ([`Self::expect_symbol_or_existing_string`]).
    pub(crate) fn expect_symbol_or_existing_to_str(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<IdentId>> {
        if self.try_symbol().is_some() || self.is_rstring().is_some() {
            return self.expect_symbol_or_existing_string(&globals.store);
        }
        match self.check_string_type(vm, globals)? {
            Some(s) => s.expect_symbol_or_existing_string(&globals.store),
            None => Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, *self)),
        }
    }

    pub(crate) fn coerce_to_rstring(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        coerce_to_rstring_inner(vm, globals, *self, &[IdentId::TO_STR])
    }

    pub(crate) fn coerce_to_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<String> {
        Ok(self.coerce_to_rstring(vm, globals)?.to_str()?.to_string())
    }

    pub(crate) fn coerce_to_str(&self, vm: &mut Executor, globals: &mut Globals) -> Result<String> {
        self.coerce_to_string(vm, globals)
    }

    /// Call Ruby-level `to_s` (explicit conversion) on the value.
    /// This is used by sprintf %s to match CRuby behavior.
    pub(crate) fn coerce_to_s(&self, vm: &mut Executor, globals: &mut Globals) -> Result<String> {
        if let Some(s) = self.is_str() {
            return Ok(s.to_string());
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::TO_S) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            if let Some(s) = result.is_str() {
                return Ok(s.to_string());
            }
        }
        Ok(self.to_s(&globals.store))
    }

    pub(crate) fn coerce_to_path_rstring(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        let rs = self.coerce_to_path_rstring_allow_nul(vm, globals)?;
        // CRuby rejects NUL in paths up front (CVE-2018-8780 hardening):
        // `ArgumentError: path name contains null byte`. Without this the
        // failed CString conversion surfaced as an opaque
        // RuntimeError("Unknown error - …").
        if rs.as_bytes().contains(&0) {
            return Err(MonorubyErr::argumenterr("path name contains null byte"));
        }
        Ok(rs)
    }

    /// `coerce_to_path_rstring` without the NUL-byte rejection, for the
    /// callers that carry their own CRuby-specific NUL message:
    /// `Dir.glob` ("nul-separated glob pattern is deprecated") and
    /// `File.join` ("string contains null byte").
    pub(crate) fn coerce_to_path_rstring_allow_nul(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<RString> {
        // CRuby's path coercion (`rb_get_path`) prefers `#to_path`, only
        // falling back to `#to_str` when `#to_path` is absent
        // (core/dir/chdir_spec.rb "prefers #to_path over #to_str").
        coerce_to_rstring_inner(vm, globals, *self, &[IdentId::TO_PATH, IdentId::TO_STR])
    }

    /// `get_pat`'s String half: the String a pattern argument that is
    /// not a Regexp stands for.
    ///
    /// A String is itself; anything else is asked for `to_str` through
    /// the full `rb_check_funcall` protocol (`rb_check_string_type`),
    /// so a `method_missing`-backed one converts and one that answers
    /// something else is the conversion mismatch. What has no `to_str`
    /// was supposed to be a Regexp, and the error says so
    /// (`Check_Type(pat, T_REGEXP)`) — not "no implicit conversion into
    /// String".
    pub(crate) fn coerce_to_pattern_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Value> {
        if self.is_rstring().is_some() {
            return Ok(*self);
        }
        match check_funcall(vm, globals, *self, IdentId::TO_STR)? {
            Some(result) if result.is_rstring().is_some() => Ok(result),
            Some(result) if !result.is_nil() => Err(MonorubyErr::cant_convert_error(
                &globals.store,
                *self,
                result,
                "String",
                IdentId::TO_STR,
            )),
            _ => Err(MonorubyErr::wrong_argument_type(
                &globals.store,
                *self,
                "Regexp",
            )),
        }
    }

    /// [`Self::coerce_to_pattern_string`], as text.
    pub(crate) fn coerce_to_pattern_str(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<String> {
        let s = self.coerce_to_pattern_string(vm, globals)?;
        Ok(s.as_rstring_inner().to_str()?.to_string())
    }

    /// `get_pat`: a Regexp as it is, and a String — or what answers
    /// `to_str` — compiled as a regexp source.
    pub(crate) fn coerce_to_regexp_or_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Regexp> {
        if let Some(re) = self.is_regex() {
            return Ok(re);
        }
        // The String is a regexp source in its own encoding — an EUC-KR
        // or UTF-16 pattern included.
        let source = self.coerce_to_pattern_string(vm, globals)?;
        Ok(Regexp::new_unchecked(Value::regexp(
            crate::builtins::regexp_inner_from_string(vm, globals, source, 0, None)?,
        )))
    }

    /// Coerce a value to a single `char` for sprintf `%c`. Returns `None`
    /// for empty strings (which produce empty output). Raises TypeError /
    /// RangeError on invalid inputs, matching CRuby's wording.
    pub(crate) fn coerce_to_char(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Option<char>> {
        match self.unpack() {
            RV::Fixnum(i) => {
                if let Ok(u) = u32::try_from(i) {
                    if let Some(c) = char::from_u32(u) {
                        return Ok(Some(c));
                    }
                }
                Err(MonorubyErr::rangeerr(format!("{i} out of char range")))
            }
            RV::Float(f) => {
                let f = f.trunc();
                if 0.0 <= f && f <= u32::MAX as f64 {
                    if let Some(c) = char::from_u32(f as u32) {
                        return Ok(Some(c));
                    }
                }
                Err(MonorubyErr::rangeerr("float out of char range"))
            }
            RV::String(s) => {
                let s = s.check_utf8()?;
                // Empty string -> empty output (no character).
                // Multi-char string -> use only the first character.
                Ok(s.chars().next())
            }
            _ => {
                // Try Integer coercion first via to_int (matches CRuby's
                // "no implicit conversion of X into Integer" error).
                if let Some(func_id) = globals.check_method(*self, IdentId::TO_INT) {
                    let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
                    if let RV::Fixnum(i) = result.unpack() {
                        if let Ok(u) = u32::try_from(i) {
                            if let Some(c) = char::from_u32(u) {
                                return Ok(Some(c));
                            }
                        }
                        return Err(MonorubyErr::rangeerr(format!("{i} out of char range")));
                    }
                    return Err(MonorubyErr::typeerr(format!(
                        "can't convert {} to Integer ({}#to_int gives {})",
                        self.get_real_class_name(&globals.store),
                        self.get_real_class_name(&globals.store),
                        result.get_real_class_name(&globals.store),
                    )));
                }
                // Then try String coercion via to_str.
                if let Some(func_id) = globals.check_method(*self, IdentId::TO_STR) {
                    let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
                    if let RV::String(s) = result.unpack() {
                        let s = s.check_utf8()?;
                        return Ok(s.chars().next());
                    }
                    return Err(MonorubyErr::typeerr(format!(
                        "can't convert {} to String ({}#to_str gives {})",
                        self.get_real_class_name(&globals.store),
                        self.get_real_class_name(&globals.store),
                        result.get_real_class_name(&globals.store),
                    )));
                }
                // CRuby uses two slightly different message templates:
                //   "no implicit conversion from nil to integer"  (nil)
                //   "no implicit conversion of <Class> into Integer" (others)
                // Match both so the spec regex (`/of nil into Integer/` is
                // accepted as `from nil to integer` by mspec's
                // `raise_consistent_error`) matches CRuby exactly.
                if self.is_nil() {
                    Err(MonorubyErr::typeerr(
                        "no implicit conversion from nil to integer",
                    ))
                } else {
                    let class_name = self.builtin_class_name(&globals.store);
                    Err(MonorubyErr::typeerr(format!(
                        "no implicit conversion of {class_name} into Integer"
                    )))
                }
            }
        }
    }
}

/// CRuby's `rb_check_funcall(recv, name, 0, 0)` — the "does this object
/// convert?" probe that every `to_str` / `to_ary` / `to_int` coercion is
/// built on. `Ok(None)` means the object does not answer to `name`; the
/// caller decides what a returned value means.
///
/// A plain method-table lookup is not enough: the protocol reaches
/// `method_missing`, and a redefined `respond_to?` can veto a method
/// that does exist.
///
/// 1. a *user-redefined* `respond_to?` gates everything — a false
///    answer stops the probe even when the method exists;
/// 2. otherwise a defined method is simply called;
/// 3. with the default `respond_to?`, a *user-defined*
///    `respond_to_missing?` gates the missing-dispatch: falsy means
///    "does not answer" and `method_missing` is not consulted;
/// 4. a *user-defined* `method_missing` is then consulted with the
///    name. A `NoMethodError` it raises for that name is re-raised when
///    the object claimed to respond (CRuby's `check_funcall_failed`)
///    and otherwise means "does not answer".
pub(crate) fn check_funcall(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    name: IdentId,
) -> Result<Option<Value>> {
    check_funcall_args(vm, globals, recv, name, &[])
}

/// [`check_funcall`] with arguments (`rb_check_funcall(recv, name, argc,
/// argv)`), e.g. the `coerce` probe of a numeric comparison. The caller
/// roots `args`.
pub(crate) fn check_funcall_args(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    name: IdentId,
    args: &[Value],
) -> Result<Option<Value>> {
    // The probe that misses on every count is by far the common one,
    // and is remembered (`Store::no_conversion`).
    let class_version = Globals::class_version();
    if globals
        .store
        .no_conversion_memoized(recv.class(), name, class_version)
    {
        return Ok(None);
    }
    let respond_to = IdentId::RESPOND_TO_;
    let default_fid = |globals: &Globals, class: ClassId, name: IdentId| {
        globals
            .store
            .check_method_for_class(class, name)
            .and_then(|e| e.func_id())
    };
    let user_fid = |globals: &Globals, method: IdentId, class: ClassId| {
        let fid = globals.check_method(recv, method)?;
        // The builtin `respond_to?` is the default without asking what
        // `Object` resolves it to.
        if method == respond_to && Some(fid) == globals.store.respond_to_fid() {
            return None;
        }
        (Some(fid) != default_fid(globals, class, method)).then_some(fid)
    };
    let probe_args = [Value::symbol(name), Value::bool(true)];

    // respond: 1 = claimed to respond, 0 = refused, -1 = undetermined
    let mut respond: i8 = -1;
    // 1. A user-redefined respond_to? vetoes the whole probe.
    if let Some(fid) = user_fid(globals, respond_to, OBJECT_CLASS) {
        let responds = vm
            .invoke_func_inner(globals, fid, recv, &probe_args, None, None)?
            .as_bool();
        if !responds {
            return Ok(None);
        }
        respond = 1;
    }
    // 2. A defined method is simply called.
    if let Some(fid) = globals.check_method(recv, name) {
        return Ok(Some(
            vm.invoke_func_inner(globals, fid, recv, args, None, None)?,
        ));
    }
    // No method, and nobody was asked yet: when the rest of the
    // protocol is the builtin one too, that is the answer — and the one
    // to remember.
    if respond < 0
        && globals
            .store
            .no_conversion(recv.class(), name, class_version)
    {
        return Ok(None);
    }
    // 3. Default respond_to?: a user-defined respond_to_missing? gates
    //    the missing-dispatch.
    if respond < 0
        && let Some(fid) = user_fid(globals, IdentId::RESPOND_TO_MISSING_, OBJECT_CLASS)
    {
        let responds = vm
            .invoke_func_inner(globals, fid, recv, &probe_args, None, None)?
            .as_bool();
        if !responds {
            return Ok(None);
        }
        respond = 1;
    }
    // 4. A user-defined method_missing is consulted with the name.
    let Some(fid) = user_fid(globals, IdentId::METHOD_MISSING, BASIC_OBJECT_CLASS) else {
        if respond > 0 {
            // Claimed to respond but only the default method_missing
            // exists: dispatch for real so the NoMethodError propagates.
            return Ok(Some(
                vm.invoke_method_inner(globals, name, recv, args, None, None)?,
            ));
        }
        return Ok(None);
    };
    let mm_args: Vec<Value> = std::iter::once(Value::symbol(name))
        .chain(args.iter().copied())
        .collect();
    match vm.invoke_func_inner(globals, fid, recv, &mm_args, None, None) {
        Ok(result) => Ok(Some(result)),
        Err(err)
            if respond < 0
                && matches!(
                    &err.kind,
                    MonorubyErrKind::NotMethod { name: n, .. }
                        if n.is_none() || *n == Some(name)
                ) =>
        {
            Ok(None)
        }
        Err(err) => Err(err),
    }
}
