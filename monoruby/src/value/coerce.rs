use super::*;
use num::{BigInt, FromPrimitive, ToPrimitive};

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
        if let Some(func_id) = globals.check_method(recv, method) {
            let result = vm.invoke_func_inner(globals, func_id, recv, &[], None, None)?;
            if let Some(s) = result.is_rstring() {
                return Ok(s);
            }
            return Err(MonorubyErr::cant_convert_error(
                globals, recv, result, "String", method,
            ));
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into String",
        recv.get_real_class_name(&globals.store)
    )))
}

impl Value {
    pub(crate) fn coerce_to_array(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Array> {
        if let Some(ary) = self.try_array_ty() {
            return Ok(ary);
        } else if let Some(fid) = globals.check_method(*self, IdentId::TO_ARY) {
            let result = vm.invoke_func_inner(globals, fid, *self, &[], None, None)?;
            if let Some(ary) = result.try_array_ty() {
                return Ok(ary);
            }
            return Err(MonorubyErr::cant_convert_error_ary(globals, *self, result));
        }
        Err(MonorubyErr::no_implicit_conversion(
            globals,
            *self,
            ARRAY_CLASS,
        ))
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
                "bignum too big to convert into `long'",
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
    pub(crate) fn coerce_to_int_i64(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<i64> {
        let res = match self.unpack() {
            RV::Fixnum(_) | RV::BigInt(_) => *self,
            _ => self.coerce_to_int(vm, globals)?,
        };
        match res.unpack() {
            RV::Fixnum(i) => return Ok(i),
            RV::BigInt(b) => {
                return b
                    .to_i64()
                    .ok_or_else(|| MonorubyErr::rangeerr("bignum too big to convert into `long'"));
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
        // First try direct method lookup (fast path), then fall back to
        // invoke_method_inner which handles method_missing.
        let result = if let Some(func_id) = globals.check_method(*self, IdentId::TO_INT) {
            vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?
        } else {
            // Try via method_missing (for Mock objects etc.)
            match vm.invoke_method_inner(globals, IdentId::TO_INT, *self, &[], None, None) {
                Ok(result) => result,
                Err(_) => {
                    return Err(MonorubyErr::no_implicit_conversion(
                        globals,
                        *self,
                        INTEGER_CLASS,
                    ));
                }
            }
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
                let s = s.check_utf8()?;
                if let Ok(i) = s.parse::<i64>() {
                    return Ok(IntegerBase::Fixnum(i));
                } else if let Ok(b) = s.parse::<BigInt>() {
                    return Ok(IntegerBase::BigInt(b));
                }
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
            self.get_real_class_name(&globals.store)
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
                let s = s.check_utf8()?;
                return s.parse::<f64>().map_err(|_| {
                    MonorubyErr::argumenterr(format!("invalid value for Float(): \"{}\"", s))
                });
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
                    && let Some(func_id) = globals.check_method(*self, IdentId::TO_F)
                {
                    let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
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
        let to_hash_id = IdentId::get_id("to_hash");
        if let Some(result) =
            vm.invoke_method_if_exists(globals, to_hash_id, *self, &[], None, None)?
        {
            if let Some(h) = result.try_hash_ty() {
                return Ok(h);
            }
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {} into Hash ({}#to_hash gives {})",
                self.get_real_class_name(globals),
                self.get_real_class_name(globals),
                result.get_real_class_name(globals),
            )));
        }
        Err(MonorubyErr::no_implicit_conversion(
            &globals.store,
            *self,
            HASH_CLASS,
        ))
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
        if let Some(id) = self.try_symbol_or_string() {
            return Ok(id);
        }
        if let Some(func_id) = globals.check_method(*self, IdentId::TO_STR) {
            let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
            if let Some(s) = result.is_str() {
                return Ok(IdentId::get_id(s));
            }
            // `#to_str` returned a non-String value -> TypeError, matching
            // CRuby's "can't convert X to String (X#to_str gives Y)".
            return Err(MonorubyErr::typeerr(format!(
                "can't convert {} to String ({}#to_str gives {})",
                self.get_real_class_name(&globals.store),
                self.get_real_class_name(&globals.store),
                result.get_real_class_name(&globals.store),
            )));
        }
        Err(MonorubyErr::is_not_symbol_nor_string(&globals.store, *self))
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
        coerce_to_rstring_inner(vm, globals, *self, &[IdentId::TO_STR, IdentId::TO_PATH])
    }

    /// Like `expect_regexp_or_string` but tries `to_str` coercion for
    /// non-string/non-regexp values.
    pub(crate) fn coerce_to_regexp_or_string(
        &self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<Regexp> {
        if let Some(re) = self.is_regex() {
            Ok(re)
        } else if let Some(s) = self.is_str() {
            Ok(Regexp::new_unchecked(Value::regexp(
                RegexpInner::with_option(s, 0)?,
            )))
        } else {
            // Try to_str coercion
            if let Some(func_id) = globals.check_method(*self, IdentId::TO_STR) {
                let result = vm.invoke_func_inner(globals, func_id, *self, &[], None, None)?;
                if let Some(s) = result.is_str() {
                    return Ok(Regexp::new_unchecked(Value::regexp(
                        RegexpInner::with_option(s, 0)?,
                    )));
                }
            }
            Err(MonorubyErr::is_not_regexp_nor_string(&globals.store, *self))
        }
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
                    let class_name = self.get_real_class_name(&globals.store);
                    Err(MonorubyErr::typeerr(format!(
                        "no implicit conversion of {class_name} into Integer"
                    )))
                }
            }
        }
    }
}
