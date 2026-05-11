use super::*;
use chrono::{
    DateTime, Datelike, Duration, FixedOffset, Local, LocalResult, NaiveDate, NaiveDateTime,
    NaiveTime, Timelike, TimeZone, Utc,
};

//
// Time class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Time", TIME_CLASS, ObjTy::TIME);
    globals.define_builtin_class_funcs_with(TIME_CLASS, "new", &["now"], time_now, 0, 7, false);
    globals.define_builtin_class_funcs_with(
        TIME_CLASS,
        "local",
        &["mktime"],
        time_local,
        1,
        10,
        false,
    );
    globals.define_builtin_class_funcs_with(TIME_CLASS, "gm", &["utc"], time_gm, 1, 10, false);
    globals.define_builtin_class_func(TIME_CLASS, "now", time_now, 0);
    globals.define_builtin_class_func_with(TIME_CLASS, "at", time_at, 1, 2, false);
    globals.store[TIME_CLASS].set_alloc_func(time_alloc_func);

    globals.define_builtin_funcs(TIME_CLASS, "gmtime", &["utc"], gmtime, 0);
    globals.define_builtin_funcs(TIME_CLASS, "gmt?", &["utc?"], gmt_, 0);
    globals.define_builtin_func(TIME_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func_with(TIME_CLASS, "localtime", localtime, 0, 1, false);
    globals.define_builtin_func(TIME_CLASS, "strftime", strftime, 1);
    globals.define_builtin_func(TIME_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(TIME_CLASS, "year", year, 0);
    globals.define_builtin_funcs(TIME_CLASS, "month", &["mon"], month, 0);
    globals.define_builtin_funcs(TIME_CLASS, "day", &["mday"], day, 0);
    globals.define_builtin_func(TIME_CLASS, "yday", yday, 0);
    globals.define_builtin_func(TIME_CLASS, "wday", wday, 0);
    globals.define_builtin_func(TIME_CLASS, "hour", hour, 0);
    globals.define_builtin_func(TIME_CLASS, "min", min_, 0);
    globals.define_builtin_func(TIME_CLASS, "sec", sec_, 0);
    globals.define_builtin_func(TIME_CLASS, "usec", usec, 0);
    globals.define_builtin_funcs(TIME_CLASS, "nsec", &["tv_nsec"], nsec, 0);
    globals.define_builtin_func(TIME_CLASS, "subsec", subsec, 0);
    globals.define_builtin_funcs(TIME_CLASS, "to_i", &["tv_sec"], to_i, 0);
    globals.define_builtin_func(TIME_CLASS, "to_f", to_f, 0);
    globals.define_builtin_funcs(TIME_CLASS, "utc_offset", &["gmt_offset", "gmtoff"], utc_offset, 0);
    globals.define_builtin_func(TIME_CLASS, "-", sub, 1);
    globals.define_builtin_func(TIME_CLASS, "+", add, 1);
    globals.define_builtin_func(TIME_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(TIME_CLASS, "<", lt, 1);
    globals.define_builtin_func(TIME_CLASS, "<=", le, 1);
    globals.define_builtin_func(TIME_CLASS, ">", gt, 1);
    globals.define_builtin_func(TIME_CLASS, ">=", ge, 1);
    globals.define_builtin_func(TIME_CLASS, "==", eq, 1);
    globals.define_builtin_func(TIME_CLASS, "eql?", eql, 1);
    globals.define_builtin_func(TIME_CLASS, "sunday?", sunday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "monday?", monday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "tuesday?", tuesday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "wednesday?", wednesday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "thursday?", thursday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "friday?", friday_q, 0);
    globals.define_builtin_func(TIME_CLASS, "saturday?", saturday_q, 0);
    globals.define_builtin_funcs(TIME_CLASS, "dst?", &["isdst"], dst_q, 0);
    globals.define_builtin_func(TIME_CLASS, "zone", zone, 0);
    globals.define_builtin_funcs(TIME_CLASS, "getutc", &["getgm"], getutc, 0);
    globals.define_builtin_func_with(TIME_CLASS, "getlocal", getlocal, 0, 1, false);
    globals.define_builtin_func(TIME_CLASS, "to_a", to_a, 0);
    globals.define_builtin_funcs_with(TIME_CLASS, "iso8601", &["xmlschema"], iso8601, 0, 1, false);
    globals.define_builtin_func(TIME_CLASS, "asctime", asctime, 0);
    globals.define_builtin_func(TIME_CLASS, "ctime", asctime, 0);
    globals.define_builtin_func_with(TIME_CLASS, "floor", floor_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "ceil", ceil_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "round", round_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "deconstruct_keys", deconstruct_keys_, 1, 1, false);
    globals.define_private_builtin_func(TIME_CLASS, "_dump", _dump, 0);
    globals.define_builtin_class_func(TIME_CLASS, "_load", _load, 1);
    // Mark `_load` private at the metaclass level so
    // `Time.private_methods.include?(:_load)` is true and `Time._load(...)`
    // raises NoMethodError, matching CRuby's `private_class_method :_load`.
    let metaclass = globals.store.get_metaclass(TIME_CLASS).id();
    let _ = globals.store.change_method_visibility_for_class(
        metaclass,
        &[IdentId::get_id("_load")],
        Visibility::Private,
    );
}

///
/// ### Time#deconstruct_keys
///
/// - deconstruct_keys(keys) -> Hash
///
/// Returns a hash of component keys for pattern matching. With `nil`,
/// returns all keys (year, month, day, yday, wday, hour, min, sec,
/// subsec, dst, zone). With an Array, returns only matching Symbol keys.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/deconstruct_keys.html]
#[monoruby_builtin]
fn deconstruct_keys_(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let arg = lfp.arg(0);
    let self_ = lfp.self_val();
    let t = self_.as_time();
    let is_utc = t.is_utc();
    let (yday, wday) = match t {
        TimeInner::Local(t) => (t.ordinal(), t.weekday().num_days_from_sunday()),
        TimeInner::Utc(t) => (t.ordinal(), t.weekday().num_days_from_sunday()),
    };
    let (year, month, day, hour, min, sec, subsec_ns) = (
        t.year(), t.month(), t.day(), t.hour(), t.minute(), t.second(), t.nanosecond(),
    );
    let zone_val = if is_utc { Value::string_from_str("UTC") } else { Value::nil() };
    let subsec_val = if subsec_ns == 0 {
        Value::integer(0)
    } else {
        Value::float(subsec_ns as f64 / 1_000_000_000.0)
    };
    let pairs: [(&str, Value); 11] = [
        ("year", Value::integer(year as i64)),
        ("month", Value::integer(month as i64)),
        ("day", Value::integer(day as i64)),
        ("yday", Value::integer(yday as i64)),
        ("wday", Value::integer(wday as i64)),
        ("hour", Value::integer(hour as i64)),
        ("min", Value::integer(min as i64)),
        ("sec", Value::integer(sec as i64)),
        ("subsec", subsec_val),
        ("dst", Value::bool(false)),
        ("zone", zone_val),
    ];
    let mut map = RubyMap::default();
    if arg.is_nil() {
        for (k, v) in &pairs {
            map.insert(Value::symbol_from_str(k), *v, vm, globals)?;
        }
    } else if arg.is_array_ty() {
        let arr = arg.as_array();
        for key in arr.iter() {
            // Silently skip non-Symbol keys (matching CRuby behaviour).
            let Some(sym) = key.try_symbol() else { continue };
            let name = sym.get_name();
            if let Some((_, v)) = pairs.iter().find(|(k, _)| *k == name) {
                map.insert(*key, *v, vm, globals)?;
            }
        }
    } else {
        return Err(MonorubyErr::typeerr(format!(
            "wrong argument type {} (expected Array or nil)",
            arg.get_real_class_name(globals)
        )));
    }
    Ok(Value::hash(map))
}

fn wday_val(lfp: &Lfp) -> u32 {
    match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.weekday().num_days_from_sunday(),
        TimeInner::Utc(t) => t.weekday().num_days_from_sunday(),
    }
}

///
/// ### Time#sunday?
///
/// - sunday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/sunday=3f.html]
#[monoruby_builtin]
fn sunday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 0))
}

///
/// ### Time#monday?
///
/// - monday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/monday=3f.html]
#[monoruby_builtin]
fn monday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 1))
}

///
/// ### Time#tuesday?
///
/// - tuesday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/tuesday=3f.html]
#[monoruby_builtin]
fn tuesday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 2))
}

///
/// ### Time#wednesday?
///
/// - wednesday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/wednesday=3f.html]
#[monoruby_builtin]
fn wednesday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 3))
}

///
/// ### Time#thursday?
///
/// - thursday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/thursday=3f.html]
#[monoruby_builtin]
fn thursday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 4))
}

///
/// ### Time#friday?
///
/// - friday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/friday=3f.html]
#[monoruby_builtin]
fn friday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 5))
}

///
/// ### Time#saturday?
///
/// - saturday? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/saturday=3f.html]
#[monoruby_builtin]
fn saturday_q(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(wday_val(&lfp) == 6))
}

///
/// ### Time#dst?
///
/// - dst? -> bool
/// - isdst -> bool
///
/// Always returns `false`; monoruby does not track DST transitions.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/dst=3f.html]
#[monoruby_builtin]
fn dst_q(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(false))
}

///
/// ### Time#zone
///
/// - zone -> String | nil
///
/// Returns "UTC" for UTC times, `nil` for fixed-offset local times.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/zone.html]
#[monoruby_builtin]
fn zone(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    match lfp.self_val().as_time() {
        TimeInner::Utc(_) => Ok(Value::string_from_str("UTC")),
        TimeInner::Local(_) => Ok(Value::nil()),
    }
}

///
/// ### Time#getutc
///
/// - getutc -> Time
/// - getgm -> Time
///
/// Returns a new Time representing the same instant in UTC.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/getgm.html]
#[monoruby_builtin]
fn getutc(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let new = match lfp.self_val().as_time() {
        TimeInner::Local(t) => TimeInner::Utc(t.with_timezone(&Utc)),
        TimeInner::Utc(t) => TimeInner::Utc(*t),
    };
    Ok(Value::new_time(new))
}

///
/// ### Time#getlocal
///
/// - getlocal -> Time
/// - getlocal(utc_offset) -> Time
///
/// Returns a new Time representing the same instant in the local time
/// zone (or the given fixed offset in seconds).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/getlocal.html]
#[monoruby_builtin]
fn getlocal(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let new = if let Some(arg0) = lfp.try_arg(0) {
        // CRuby's `Time#getlocal(arg)` accepts the same offset shapes as
        // `Time.new`'s `utc_offset` slot: Integer / Rational / Float
        // seconds, or `"+HH:MM"` / `"+HH:MM:SS"` String. Also accepts
        // a `to_int` Mock object (handled by `parse_utc_offset` falling
        // through to `coerce_to_int_i64`).
        let fixed = parse_utc_offset(vm, globals, arg0)?;
        match lfp.self_val().as_time() {
            TimeInner::Local(t) => TimeInner::Local(t.with_timezone(&fixed)),
            TimeInner::Utc(t) => TimeInner::Local(t.with_timezone(&fixed)),
        }
    } else {
        match lfp.self_val().as_time() {
            TimeInner::Local(t) => {
                let local: DateTime<Local> = (*t).into();
                TimeInner::Local(local.into())
            }
            TimeInner::Utc(t) => {
                let local: DateTime<Local> = (*t).into();
                TimeInner::Local(local.into())
            }
        }
    };
    Ok(Value::new_time(new))
}

///
/// ### Time#to_a
///
/// - to_a -> Array
///
/// Returns `[sec, min, hour, day, month, year, wday, yday, isdst, zone]`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/to_a.html]
#[monoruby_builtin]
fn to_a(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let t = self_.as_time();
    let is_utc = t.is_utc();
    let (wday, yday) = match t {
        TimeInner::Local(t) => (t.weekday().num_days_from_sunday(), t.ordinal()),
        TimeInner::Utc(t) => (t.weekday().num_days_from_sunday(), t.ordinal()),
    };
    let (sec, min, hour, day, mon, year) = (
        t.second(), t.minute(), t.hour(), t.day(), t.month(), t.year(),
    );
    let zone = if is_utc {
        Value::string_from_str("UTC")
    } else {
        Value::nil()
    };
    Ok(Value::array_from_vec(vec![
        Value::integer(sec as _),
        Value::integer(min as _),
        Value::integer(hour as _),
        Value::integer(day as _),
        Value::integer(mon as _),
        Value::integer(year as _),
        Value::integer(wday as _),
        Value::integer(yday as _),
        Value::bool(false),
        zone,
    ]))
}

///
/// ### Time#_dump (private)
///
/// - _dump(*) -> String
///
/// Marshal-format hook. Returns the 8-byte little-endian
/// representation CRuby uses for `Marshal.dump(time)`. Layout
/// (CRuby `time.c`, `time_mdump`):
///
/// ```text
/// high u32:
///   bit 31      : 1 (new format marker)
///   bit 30      : 1 if UTC, else 0
///   bits 14..29 : year - 1900
///   bits 10..13 : month - 1
///   bits 5..9   : mday
///   bits 0..4   : hour
/// low u32:
///   bits 26..31 : min
///   bits 20..25 : sec
///   bits 0..19  : usec
/// ```
///
/// Sub-microsecond precision and non-UTC offsets are not encoded;
/// matches CRuby's "old marshal format" path that doesn't add the
/// trailing extension blob.
#[monoruby_builtin]
fn _dump(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let t = self_.as_time();
    let (year, month, day, hour, min, sec, usec, is_utc) = (
        t.year(),
        t.month(),
        t.day(),
        t.hour(),
        t.minute(),
        t.second(),
        t.nanosecond() / 1000,
        t.is_utc(),
    );
    let high: u32 = (1u32 << 31)
        | ((is_utc as u32) << 30)
        | (((year - 1900) as u32 & 0xFFFF) << 14)
        | ((month - 1) << 10)
        | (day << 5)
        | hour;
    let low: u32 = (min << 26) | (sec << 20) | (usec & 0xFFFFF);
    let mut bytes = Vec::with_capacity(8);
    bytes.extend_from_slice(&high.to_le_bytes());
    bytes.extend_from_slice(&low.to_le_bytes());
    Ok(Value::bytes(bytes))
}

///
/// ### Time._load (private class method)
///
/// - _load(string) -> Time
///
/// Reverse of `Time#_dump`. Accepts both the "new" format (bit 31
/// of the high word is set) and the legacy UNIX-timestamp format
/// (bit 31 clear; high = epoch seconds, low = usec).
#[monoruby_builtin]
fn _load(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let arg = lfp.arg(0);
    let bytes = arg.expect_bytes(&globals.store)?;
    if bytes.len() != 8 {
        return Err(MonorubyErr::typeerr("marshaled time format error".to_string()));
    }
    let high = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    let low = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
    if (high >> 31) & 1 == 0 {
        // Legacy UNIX-timestamp format: high = secs, low = usec.
        let secs = high as i64;
        let nsec = low.saturating_mul(1000);
        let dt = DateTime::<Utc>::from_timestamp(secs, nsec).ok_or_else(|| {
            MonorubyErr::argumenterr("marshaled time data has out-of-range secs")
        })?;
        return Ok(Value::new_time(TimeInner::Utc(dt)));
    }
    let is_utc = (high >> 30) & 1 == 1;
    let year = ((high >> 14) & 0xFFFF) as i32 + 1900;
    let month = (high >> 10) & 0xF;
    let day = (high >> 5) & 0x1F;
    let hour = high & 0x1F;
    let min = (low >> 26) & 0x3F;
    let sec = (low >> 20) & 0x3F;
    let usec = low & 0xFFFFF;
    let naive = NaiveDate::from_ymd_opt(year, month + 1, day)
        .and_then(|d| d.and_hms_micro_opt(hour, min, sec, usec))
        .ok_or_else(|| MonorubyErr::argumenterr("marshaled time data out of range"))?;
    if is_utc {
        Ok(Value::new_time(TimeInner::Utc(Utc.from_utc_datetime(&naive))))
    } else {
        let local = match Local.from_local_datetime(&naive) {
            LocalResult::Single(t) => t,
            LocalResult::Ambiguous(t, _) => t,
            LocalResult::None => {
                return Err(MonorubyErr::argumenterr(
                    "marshaled time data does not exist in local time",
                ))
            }
        };
        Ok(Value::new_time(TimeInner::Local(local.into())))
    }
}

///
/// ### Time#iso8601
///
/// - iso8601(fraction_digits = 0) -> String
/// - xmlschema(fraction_digits = 0) -> String
///
/// Returns an ISO 8601 / XML Schema representation. With a positive
/// `fraction_digits` argument, appends sub-second precision —
/// `t.iso8601(2)` → `"2000-01-02T03:04:05.52+09:00"`. The year is
/// emitted with at least four digits and may be longer (`12` → `0012`,
/// `40000` → `40000`), matching CRuby ≥ 3.4.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/iso8601.html]
#[monoruby_builtin]
fn iso8601(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let digits = if let Some(a) = lfp.try_arg(0) {
        let v = a.coerce_to_int_i64(vm, globals)?;
        if !(0..=9).contains(&v) {
            return Err(MonorubyErr::argumenterr("fraction_digits out of range"));
        }
        v as u32
    } else {
        0
    };
    let self_ = lfp.self_val();
    let t = self_.as_time();
    let suffix = match t {
        TimeInner::Local(t) => t.format("%:z").to_string(),
        TimeInner::Utc(_) => "Z".to_string(),
    };
    let (year, mon, mday, h, mi, s, nsec) = (
        t.year(), t.month(), t.day(), t.hour(), t.minute(), t.second(), t.nanosecond(),
    );
    let year_str = if year < 0 {
        format!("-{:04}", -year)
    } else {
        format!("{:04}", year)
    };
    let mut out = format!(
        "{}-{:02}-{:02}T{:02}:{:02}:{:02}",
        year_str, mon, mday, h, mi, s
    );
    if digits > 0 {
        let frac = nsec / 10u32.pow(9 - digits);
        out.push('.');
        out.push_str(&format!("{:0width$}", frac, width = digits as usize));
    }
    out.push_str(&suffix);
    Ok(Value::string(out))
}

///
/// ### Time#asctime
///
/// - asctime -> String
/// - ctime -> String
///
/// Returns the canonical `strftime("%a %b %e %H:%M:%S %Y")` form.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/asctime.html]
#[monoruby_builtin]
fn asctime(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format("%a %b %e %H:%M:%S %Y").to_string(),
        TimeInner::Utc(t) => t.format("%a %b %e %H:%M:%S %Y").to_string(),
    };
    Ok(Value::string(s))
}

fn precision_arg(vm: &mut Executor, globals: &mut Globals, lfp: &Lfp) -> Result<u32> {
    if let Some(a) = lfp.try_arg(0) {
        let v = a.coerce_to_int_i64(vm, globals)?;
        if !(0..=9).contains(&v) {
            return Err(MonorubyErr::argumenterr("precision out of range"));
        }
        Ok(v as u32)
    } else {
        Ok(0)
    }
}

fn rescale_nsec(ns: u32, precision: u32, mode: i8) -> u32 {
    // mode: -1 floor, 0 round, 1 ceil
    if precision >= 9 {
        return ns;
    }
    let div = 10u32.pow(9 - precision);
    let q = ns / div;
    let r = ns % div;
    let q = match mode {
        -1 => q,
        1 => if r == 0 { q } else { q + 1 },
        _ => if r * 2 >= div { q + 1 } else { q },
    };
    q * div
}

fn apply_subsec(lfp: &Lfp, mode: i8, precision: u32) -> TimeInner {
    let self_ = lfp.self_val();
    let inner = self_.as_time();
    let new_ns = rescale_nsec(inner.nanosecond(), precision, mode);
    match inner {
        TimeInner::Local(t) => {
            let mut result = t.with_nanosecond(0).unwrap();
            if new_ns >= 1_000_000_000 {
                result = result + Duration::seconds(1);
            } else {
                result = result.with_nanosecond(new_ns).unwrap();
            }
            TimeInner::Local(result)
        }
        TimeInner::Utc(t) => {
            let mut result = t.with_nanosecond(0).unwrap();
            if new_ns >= 1_000_000_000 {
                result = result + Duration::seconds(1);
            } else {
                result = result.with_nanosecond(new_ns).unwrap();
            }
            TimeInner::Utc(result)
        }
    }
}

///
/// ### Time#floor
///
/// - floor(precision = 0) -> Time
///
/// Rounds sub-seconds toward minus infinity at the given decimal precision
/// (0..9). With `precision = 0`, truncates sub-seconds entirely.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/floor.html]
#[monoruby_builtin]
fn floor_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let p = precision_arg(vm, globals, &lfp)?;
    Ok(Value::new_time(apply_subsec(&lfp, -1, p)))
}

///
/// ### Time#ceil
///
/// - ceil(precision = 0) -> Time
///
/// Rounds sub-seconds toward plus infinity at the given decimal precision.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/ceil.html]
#[monoruby_builtin]
fn ceil_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let p = precision_arg(vm, globals, &lfp)?;
    Ok(Value::new_time(apply_subsec(&lfp, 1, p)))
}

///
/// ### Time#round
///
/// - round(precision = 0) -> Time
///
/// Rounds sub-seconds half-up at the given decimal precision.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/round.html]
#[monoruby_builtin]
fn round_(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let p = precision_arg(vm, globals, &lfp)?;
    Ok(Value::new_time(apply_subsec(&lfp, 0, p)))
}

///
/// ### Time.new / Time.now
/// - new -> Time                         (current time, local zone)
/// - new(year, ...) -> Time              (1..7 args; last is `utc_offset`)
/// - now -> Time                         (alias for the no-arg form)
///
/// `Time.new` differs from `Time.gm` / `Time.local` in two ways:
/// - the 7th positional argument is `utc_offset` (Integer seconds /
///   Rational / `"+HH:MM"` / `"+HH:MM:SS"` String), **not** `usec`;
/// - omitting `utc_offset` (`Time.new(2024, 1, 1)`) builds a local
///   time, matching CRuby.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/new.html]
#[monoruby_builtin]
fn time_now(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `Time.now` is registered separately with arity 0, so its frame
    // has no arg slots — `lfp.try_arg(i)` would read past the
    // allocated frame. Bail to "now" when `arg_len() == 0` *before*
    // touching any arg slot.
    if lfp.arg_len() == 0 || (0..7).all(|i| lfp.try_arg(i).is_none()) {
        let time_info = TimeInner::Local(Local::now().into());
        return Ok(Value::new_time(time_info));
    }
    // Build via the shared `from_args` path. `Time.new`'s 7th arg is
    // `utc_offset`, so we pre-extract it before calling `from_args`
    // (which expects `usec` in slot 6).
    let utc_offset_arg = lfp.try_arg(6);
    let naive = from_args_skip_last(vm, globals, lfp)?
        .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?;
    let time_info = if let Some(off_arg) = utc_offset_arg {
        if off_arg.is_nil() {
            // Same shape as `Time.local(year, …)` — local-zone time.
            let local = match Local.from_local_datetime(&naive) {
                LocalResult::Single(t) => t,
                LocalResult::Ambiguous(t, _) => t,
                LocalResult::None => {
                    return Err(MonorubyErr::argumenterr("argument out of range."))
                }
            };
            TimeInner::Local(local.into())
        } else {
            let offset = parse_utc_offset(vm, globals, off_arg)?;
            let dt = offset
                .from_local_datetime(&naive)
                .single()
                .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?;
            TimeInner::Local(dt)
        }
    } else {
        let local = match Local.from_local_datetime(&naive) {
            LocalResult::Single(t) => t,
            LocalResult::Ambiguous(t, _) => t,
            LocalResult::None => {
                return Err(MonorubyErr::argumenterr("argument out of range."))
            }
        };
        TimeInner::Local(local.into())
    };
    Ok(Value::new_time(time_info))
}

/// Variant of `from_args` used by `Time.new`: the 7th positional arg
/// is consumed by the caller as `utc_offset`, not as `usec`. Re-uses
/// every other slot's coercion (String → numeric, nil-default,
/// month-name lookup, fractional sec → nsec, day/sec carry-over).
fn from_args_skip_last(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
) -> Result<Option<NaiveDateTime>> {
    // Wrap with a synthetic Lfp would be ideal, but the helper takes a
    // real `Lfp`. Inline the body of `from_args` here, dropping the
    // final `usec_arg` slot.
    let year_arg = lfp.try_arg(0);
    let mon_arg = lfp.try_arg(1);
    let day_arg = lfp.try_arg(2);
    let hour_arg = lfp.try_arg(3);
    let min_arg = lfp.try_arg(4);
    let sec_arg = lfp.try_arg(5);

    let year = match year_arg {
        Some(v) => match i32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        None => return Err(MonorubyErr::typeerr("no implicit conversion of nil into Integer")),
    };
    let mon = match mon_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_month_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 1,
    };
    let day = match day_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 1,
    };
    let hour = match hour_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 0,
    };
    let min = match min_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 0,
    };
    let (sec, sec_fractional_nsec) = match sec_arg {
        Some(v) if !v.is_nil() => time_sec_to_i64_nsec(vm, globals, v)?,
        _ => (0, 0),
    };
    let sec_u32 = match u32::try_from(sec) {
        Ok(i) => i,
        Err(_) => return Ok(None),
    };
    Ok(Some(build_naive_datetime(
        year,
        mon,
        day,
        hour,
        min,
        sec_u32,
        sec_fractional_nsec,
    )?))
}

/// Parse a `utc_offset` argument. Accepts:
/// - Integer / Rational / `to_int`-respondent → seconds east of UTC;
/// - `"+HH:MM"` / `"-HH:MM"` / `"+HH:MM:SS"` / `"-HH:MM:SS"` strings
///   (also `to_str`-respondent).
/// Validates ASCII-only and per-field ranges so monoruby surfaces the
/// same `ArgumentError` shape as CRuby for the `localtime("xxx")`
/// edge cases.
fn parse_utc_offset(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<FixedOffset> {
    if let Some(s) = v.is_str() {
        return parse_utc_offset_string(s);
    }
    // Integer / Rational / Float / to_int
    if let Some(f) = v.try_float() {
        let secs = f.trunc() as i32;
        return FixedOffset::east_opt(secs)
            .ok_or_else(|| MonorubyErr::argumenterr("utc_offset out of range"));
    }
    let secs = match i32::try_from(v.coerce_to_int_i64(vm, globals)?) {
        Ok(s) => s,
        Err(_) => {
            return Err(MonorubyErr::argumenterr("utc_offset out of range"));
        }
    };
    FixedOffset::east_opt(secs)
        .ok_or_else(|| MonorubyErr::argumenterr("utc_offset out of range"))
}

fn parse_utc_offset_string(s: &str) -> Result<FixedOffset> {
    if !s.is_ascii() {
        return Err(MonorubyErr::argumenterr("string must be ASCII-compatible"));
    }
    let bytes = s.as_bytes();
    let sign = match bytes.first() {
        Some(b'+') => 1,
        Some(b'-') => -1,
        _ => return Err(MonorubyErr::argumenterr(format!(
            r#""+HH:MM" or "-HH:MM" expected for utc_offset"#
        ))),
    };
    let parse2 = |idx: usize| -> Result<i32> {
        if bytes.len() < idx + 2 {
            return Err(MonorubyErr::argumenterr(format!(
                r#""+HH:MM" or "-HH:MM" expected for utc_offset"#
            )));
        }
        let pair = &bytes[idx..idx + 2];
        if !pair[0].is_ascii_digit() || !pair[1].is_ascii_digit() {
            return Err(MonorubyErr::argumenterr(format!(
                r#""+HH:MM" or "-HH:MM" expected for utc_offset"#
            )));
        }
        Ok(((pair[0] - b'0') as i32) * 10 + (pair[1] - b'0') as i32)
    };
    let h = parse2(1)?;
    if h > 23 {
        return Err(MonorubyErr::argumenterr("utc_offset out of range"));
    }
    if bytes.get(3) != Some(&b':') {
        return Err(MonorubyErr::argumenterr(format!(
            r#""+HH:MM" or "-HH:MM" expected for utc_offset"#
        )));
    }
    let m = parse2(4)?;
    if m > 59 {
        return Err(MonorubyErr::argumenterr("utc_offset out of range"));
    }
    let s_secs = if bytes.len() == 9 {
        if bytes.get(6) != Some(&b':') {
            return Err(MonorubyErr::argumenterr(format!(
                r#""+HH:MM:SS" expected for utc_offset"#
            )));
        }
        let s = parse2(7)?;
        if s > 59 {
            return Err(MonorubyErr::argumenterr("utc_offset out of range"));
        }
        s
    } else if bytes.len() == 6 {
        0
    } else {
        return Err(MonorubyErr::argumenterr(format!(
            r#""+HH:MM" or "-HH:MM" expected for utc_offset"#
        )));
    };
    let total = sign * (h * 3600 + m * 60 + s_secs);
    FixedOffset::east_opt(total)
        .ok_or_else(|| MonorubyErr::argumenterr("utc_offset out of range"))
}

///
/// ### Time.at
/// - at(time) -> Time
/// - at(time, usec) -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/at.html]
#[monoruby_builtin]
fn time_at(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let secs_val = lfp.arg(0);
    let (secs, nsecs) = if let Some(f) = secs_val.try_float() {
        let s = f.floor() as i64;
        let ns = ((f - f.floor()) * 1_000_000_000.0) as u32;
        (s, ns)
    } else {
        let s = secs_val.coerce_to_int_i64(vm, globals)?;
        (s, 0u32)
    };
    let usec_ns = if let Some(arg1) = lfp.try_arg(1) {
        let u = arg1.coerce_to_int_i64(vm, globals)?;
        (u * 1000) as u32
    } else {
        0
    };
    let total_ns = nsecs + usec_ns;
    let dt = DateTime::from_timestamp(secs, total_ns)
        .ok_or_else(|| MonorubyErr::argumenterr("out of Time range"))?;
    let local: DateTime<Local> = dt.into();
    let time_info = TimeInner::Local(local.into());
    Ok(Value::new_time(time_info))
}

/// Allocator for `Time` and its subclasses.
pub(crate) extern "C" fn time_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    let time_info = TimeInner::Utc(DateTime::<Utc>::default());
    Value::new_time_with_class(time_info, class_id)
}

///
/// ### Time.local
/// - local(year, mon = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> Time
/// - mktime(year, mon = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/local.html]
#[monoruby_builtin]
fn time_local(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let t = generate_time(vm, globals, Local, lfp)?;
    let time_info = TimeInner::Local(t.into());
    Ok(Value::new_time(time_info))
}

///
/// ### Time.gm
/// - gm(year, mon = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> Time
/// - utc(year, mon = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> Time
///
/// [utc(year, mon = 1, day = 1, hour = 0, min = 0, sec = 0, usec = 0) -> Time]
#[monoruby_builtin]
fn time_gm(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let t = generate_time(vm, globals, Utc, lfp)?;
    let time_info = TimeInner::Utc(t);
    Ok(Value::new_time(time_info))
}

fn from_args(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Option<NaiveDateTime>> {
    // Reorder args when the 10-arg C-style form is used:
    // `Time.gm(sec, min, hour, mday, mon, year, wday, yday, isdst, tz)`.
    // The trailing four args (wday, yday, isdst, tz) are intentionally
    // ignored — CRuby treats them as advisory.
    let arg_count = (0..10).filter(|i| lfp.try_arg(*i).is_some()).count();
    let (year_arg, mon_arg, day_arg, hour_arg, min_arg, sec_arg, usec_arg) = if arg_count == 10 {
        (
            lfp.try_arg(5),
            lfp.try_arg(4),
            lfp.try_arg(3),
            lfp.try_arg(2),
            lfp.try_arg(1),
            lfp.try_arg(0),
            None,
        )
    } else if arg_count >= 8 {
        return Err(MonorubyErr::argumenterr(format!(
            "wrong number of arguments (given {}, expected 1..7)",
            arg_count
        )));
    } else {
        (
            lfp.try_arg(0),
            lfp.try_arg(1),
            lfp.try_arg(2),
            lfp.try_arg(3),
            lfp.try_arg(4),
            lfp.try_arg(5),
            lfp.try_arg(6),
        )
    };

    let year = match year_arg {
        Some(v) => match i32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        None => return Err(MonorubyErr::typeerr("no implicit conversion of nil into Integer")),
    };
    let mon = match mon_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_month_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 1,
    };
    let day = match day_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 1,
    };
    let hour = match hour_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 0,
    };
    let min = match min_arg {
        Some(v) if !v.is_nil() => match u32::try_from(time_arg_to_i64(vm, globals, v)?) {
            Ok(i) => i,
            Err(_) => return Ok(None),
        },
        _ => 0,
    };
    // `sec` accepts Float / Rational and cascades the fractional part
    // down into nanoseconds (CRuby semantics).
    // `Time.gm(2000,1,1,20,15,1.75)` yields `sec=1, usec=750_000`,
    // and `Time.gm(.., "25.0123456789".to_r)` preserves all 9 fractional
    // digits — used by `Time#ceil(N)` / `Time#round(N)` specs.
    // An explicit `usec`/sub-sec argument overrides the cascade
    // *including its own fractional remainder added to nsec*.
    let (sec, sec_fractional_nsec) = match sec_arg {
        Some(v) if !v.is_nil() => time_sec_to_i64_nsec(vm, globals, v)?,
        _ => (0, 0),
    };
    let nsec: u32 = match usec_arg {
        Some(v) if !v.is_nil() => match time_usec_to_nsec(vm, globals, v)? {
            Some(u) => u,
            None => return Ok(None),
        },
        _ => sec_fractional_nsec,
    };
    let sec_u32 = match u32::try_from(sec) {
        Ok(i) => i,
        Err(_) => return Ok(None),
    };
    Ok(Some(build_naive_datetime(year, mon, day, hour, min, sec_u32, nsec)?))
}

/// Build a `NaiveDateTime` honouring CRuby's "carry forward" rules:
/// - `sec == 60` carries to `+1` minute (leap-second tolerance);
/// - a `mday` that exceeds the month's actual length carries into the
///   next month, provided `mday <= 31` (the spec's hard ceiling — `day=32`
///   is rejected even though `Dec 32` would arithmetically be Jan 1).
fn build_naive_datetime(
    year: i32,
    mon: u32,
    day: u32,
    hour: u32,
    min: u32,
    sec: u32,
    nsec: u32,
) -> Result<NaiveDateTime> {
    if !(1..=12).contains(&mon) {
        return Err(MonorubyErr::argumenterr("argument out of range."));
    }
    if !(1..=31).contains(&day) {
        return Err(MonorubyErr::argumenterr("argument out of range."));
    }
    if hour > 23 || min > 59 || sec > 60 || nsec >= 1_000_000_000 {
        return Err(MonorubyErr::argumenterr("argument out of range."));
    }
    // `sec == 60` builds with sec=0 and then adds one minute, matching
    // CRuby's leap-second normalisation. Carrying past midnight cascades
    // through `chrono::Duration` so day/month/year roll over correctly.
    let (sec_real, leap_carry) = if sec == 60 { (0, true) } else { (sec, false) };
    let date = match NaiveDate::from_ymd_opt(year, mon, day) {
        Some(d) => d,
        None => {
            // Day exceeds days-in-month; carry the excess into the next
            // month if `day <= 31`.
            let dim = days_in_month(year, mon);
            if day <= dim {
                // shouldn't happen — `from_ymd_opt` only fails for invalid combos.
                return Err(MonorubyErr::argumenterr("argument out of range."));
            }
            let excess = day - dim;
            let (next_year, next_mon) = if mon == 12 {
                (year + 1, 1)
            } else {
                (year, mon + 1)
            };
            NaiveDate::from_ymd_opt(next_year, next_mon, excess)
                .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?
        }
    };
    let time = NaiveTime::from_hms_nano_opt(hour, min, sec_real, nsec)
        .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?;
    let mut dt = NaiveDateTime::new(date, time);
    if leap_carry {
        dt = dt
            .checked_add_signed(Duration::minutes(1))
            .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?;
    }
    Ok(dt)
}

fn days_in_month(year: i32, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            let leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
            if leap { 29 } else { 28 }
        }
        _ => 0,
    }
}

/// Coerce a `Time.gm` / `Time.local` numeric argument to `i64`.
/// Accepts Integer / Float / `to_int`-respondent objects (delegated
/// to `coerce_to_int_i64`) as well as numeric Strings — CRuby parses
/// `"2000"` / `"08"` as base-10 integers here. Surrounding whitespace
/// is stripped (matching `String#to_i`).
fn time_arg_to_i64(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<i64> {
    if let Some(s) = v.is_str() {
        let trimmed = s.trim();
        return trimmed.parse::<i64>().map_err(|_| {
            MonorubyErr::argumenterr(format!("argument out of range: {:?}", trimmed))
        });
    }
    v.coerce_to_int_i64(vm, globals)
}

/// Coerce the `mon` argument to an `i64` month number. Same rules as
/// `time_arg_to_i64` plus a short-name lookup: `"jan"`/`"Jan"` → 1,
/// `"dec"` → 12, etc. Names that don't match still fall through to the
/// numeric parse so `"12"` works.
fn time_month_to_i64(vm: &mut Executor, globals: &mut Globals, v: Value) -> Result<i64> {
    if let Some(s) = v.is_str() {
        let trimmed = s.trim();
        if let Some(n) = month_name_to_num(trimmed) {
            return Ok(n);
        }
        return trimmed.parse::<i64>().map_err(|_| {
            MonorubyErr::argumenterr(format!("argument out of range: {:?}", trimmed))
        });
    }
    v.coerce_to_int_i64(vm, globals)
}

/// Coerce the `sec` argument to `(integer_seconds, fractional_nsec)`.
/// `Float` / `Rational` cascade their fractional part into nanoseconds
/// that propagate to the caller, matching CRuby's
/// `Time.gm(2000, 1, 1, 0, 0, 1.75)` → `sec=1, usec=750_000` (and
/// the analogous nsec case for high-precision Rationals like
/// `"25.0123456789".to_r`).
fn time_sec_to_i64_nsec(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<(i64, u32)> {
    if let Some(f) = v.try_float() {
        let secs = f.trunc() as i64;
        let frac = f - f.trunc();
        let nsec = (frac * 1_000_000_000.0).round() as i64;
        return Ok((secs, nsec.clamp(0, 999_999_999) as u32));
    }
    if let Some(r) = v.try_rational() {
        return Ok(rational_split_sec_nsec(r));
    }
    if let Some(s) = v.is_str() {
        let trimmed = s.trim();
        return Ok((
            trimmed.parse::<i64>().map_err(|_| {
                MonorubyErr::argumenterr(format!("argument out of range: {:?}", trimmed))
            })?,
            0,
        ));
    }
    Ok((v.coerce_to_int_i64(vm, globals)?, 0))
}

/// Split a `Rational` into `(integer_part, fractional_nanoseconds)`
/// using BigInt arithmetic so values like `"25.0123456789".to_r`
/// survive full nine-digit precision (Float intermediate would
/// truncate at ~16 significant digits).
fn rational_split_sec_nsec(r: &RationalInner) -> (i64, u32) {
    use num::Integer;
    let num = r.num();
    let den = r.den();
    let (sec_big, rem) = num.div_mod_floor(den);
    let secs = sec_big.to_i64().unwrap_or(0);
    let nsec_big = (&rem * num::BigInt::from(1_000_000_000i64)) / den;
    let nsec = nsec_big.to_i64().unwrap_or(0).clamp(0, 999_999_999) as u32;
    (secs, nsec)
}

/// Coerce a `usec` argument (microseconds) to nanoseconds for the
/// internal `NaiveTime` representation. Float / Rational fractions
/// extend below the microsecond boundary — `Time.gm(.., 1.75)` for
/// the usec slot stores `1_750` ns.
fn time_usec_to_nsec(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Result<Option<u32>> {
    if let Some(f) = v.try_float() {
        let nsec = (f * 1_000.0).round() as i64;
        return Ok(u32::try_from(nsec).ok());
    }
    if let Some(r) = v.try_rational() {
        use num::Integer;
        let num = r.num() * num::BigInt::from(1_000i64);
        let den = r.den();
        let val = num.div_floor(den).to_i64().unwrap_or(-1);
        return Ok(u32::try_from(val).ok());
    }
    let i = time_arg_to_i64(vm, globals, v)?;
    Ok(u32::try_from(i.saturating_mul(1_000)).ok())
}

fn month_name_to_num(s: &str) -> Option<i64> {
    const NAMES: [&str; 12] = [
        "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
    ];
    if s.len() != 3 {
        return None;
    }
    let lower = s.to_ascii_lowercase();
    NAMES
        .iter()
        .position(|n| *n == lower.as_str())
        .map(|i| (i + 1) as i64)
}

fn generate_time<Tz: TimeZone>(vm: &mut Executor, globals: &mut Globals, tz: Tz, lfp: Lfp) -> Result<DateTime<Tz>> {
    let naive =
        from_args(vm, globals, lfp)?.ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?;
    Ok(match naive.and_local_timezone(tz) {
        LocalResult::Single(t) => t,
        _ => return Err(MonorubyErr::argumenterr("argument out of range.")),
    })
}

///
/// ### Time#gmt?
/// - gmt? -> bool
/// - utc? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/gmt=3f.html]
#[monoruby_builtin]
fn gmt_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let b = lfp.self_val().as_time().is_utc();
    Ok(Value::bool(b))
}

///
/// ### Time#gmtime
/// - gmt -> self
/// - utc -> self
///
/// A frozen time that's already UTC is a no-op (CRuby short-circuits
/// before the frozen check); a frozen time in a non-UTC zone raises
/// `FrozenError`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/gmtime.html]
#[monoruby_builtin]
fn gmtime(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    if self_val.as_time().is_utc() {
        return Ok(self_val);
    }
    self_val.ensure_not_frozen(&globals.store)?;
    self_val.as_time_mut().utc();
    Ok(self_val)
}

///
/// ### Time#localtime
/// - localtime -> self
/// - localtime(utc_offset) -> self
///
/// Without an argument, converts a UTC time to the system's local
/// zone (no-op for already-local). With an argument, shifts to a
/// fixed offset — accepts the same shapes as `Time.new`'s
/// `utc_offset` slot (Integer / Float / Rational seconds, or
/// `"+HH:MM"` / `"+HH:MM:SS"` String).
///
/// `Time#localtime` on a frozen time that's already in the requested
/// zone is a no-op; otherwise it raises `FrozenError`, matching CRuby.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/localtime.html]
#[monoruby_builtin]
fn localtime(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_val = lfp.self_val();
    if let Some(arg0) = lfp.try_arg(0) {
        let target = parse_utc_offset(vm, globals, arg0)?;
        // Already in the requested zone? No-op (matches CRuby's
        // short-circuit before the frozen check).
        if let TimeInner::Local(t) = self_val.as_time() {
            if t.offset() == &target {
                return Ok(self_val);
            }
        }
        self_val.ensure_not_frozen(&globals.store)?;
        self_val.as_time_mut().shift_to_offset(target);
        return Ok(self_val);
    }
    if !self_val.as_time().is_utc() {
        // Already local, no offset arg → no-op.
        return Ok(self_val);
    }
    self_val.ensure_not_frozen(&globals.store)?;
    self_val.as_time_mut().local();
    Ok(self_val)
}

///
/// ### Time#inspect
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/inspect.html]
#[monoruby_builtin]
fn inspect(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let time = lfp.self_val().as_time().to_string();
    Ok(Value::string(time))
}

///
/// ### Time#strftime
/// - strftime(format) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/strftime.html]
#[monoruby_builtin]
fn strftime(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fmt_str = lfp.arg(0).coerce_to_str(vm, globals)?;

    // Get nanoseconds from the time value for Ruby-specific format specifiers.
    let nanos = lfp.self_val().as_time().nanosecond();

    // Replace Ruby-specific nanosecond specifiers that chrono doesn't support.
    let fmt = fmt_str
        .replace("%9N", &format!("{:09}", nanos))
        .replace("%N", &format!("{:09}", nanos))
        .replace("%6N", &format!("{:06}", nanos / 1_000))
        .replace("%3N", &format!("{:03}", nanos / 1_000_000))
        .replace("%L", &format!("{:03}", nanos / 1_000_000));

    use std::fmt::Write;
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => {
            let mut result = String::new();
            let _ = write!(result, "{}", t.format(&fmt));
            result
        }
        TimeInner::Utc(t) => {
            let mut result = String::new();
            let _ = write!(result, "{}", t.format(&fmt));
            result
        }
    };
    Ok(Value::string(s))
}

///
/// ### Time#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/to_s.html]
#[monoruby_builtin]
fn to_s(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format("%Y-%m-%d %H:%M:%S %z"),
        TimeInner::Utc(t) => t.format("%Y-%m-%d %H:%M:%S UTC"),
    }
    .to_string();
    Ok(Value::string(s))
}

///
/// ### Time#year
/// - year -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/year.html]
#[monoruby_builtin]
fn year(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().year() as _))
}

///
/// ### Time#month
/// - mon -> Integer
/// - month -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/mon.html]
#[monoruby_builtin]
fn month(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().month() as _))
}

///
/// ### Time#day
/// - mday -> Integer
/// - day -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/day.html]
#[monoruby_builtin]
fn day(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().day() as _))
}

///
/// ### Time#yday
/// - yday -> Integer
///
/// Returns the day of the year (1..366).
#[monoruby_builtin]
fn yday(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let d = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.ordinal(),
        TimeInner::Utc(t) => t.ordinal(),
    };
    Ok(Value::integer(d as _))
}

///
/// ### Time#wday
/// - wday -> Integer
///
/// Returns the day of the week (0=Sunday .. 6=Saturday).
#[monoruby_builtin]
fn wday(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let w = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.weekday().num_days_from_sunday(),
        TimeInner::Utc(t) => t.weekday().num_days_from_sunday(),
    };
    Ok(Value::integer(w as _))
}

///
/// ### Time#hour
#[monoruby_builtin]
fn hour(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().hour() as _))
}

///
/// ### Time#min
#[monoruby_builtin]
fn min_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().minute() as _))
}

///
/// ### Time#sec
#[monoruby_builtin]
fn sec_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().second() as _))
}

///
/// ### Time#usec
#[monoruby_builtin]
fn usec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer((lfp.self_val().as_time().nanosecond() / 1_000) as _))
}

///
/// ### Time#nsec
#[monoruby_builtin]
fn nsec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::integer(lfp.self_val().as_time().nanosecond() as _))
}

///
/// ### Time#subsec
#[monoruby_builtin]
fn subsec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = lfp.self_val().as_time().nanosecond();
    // Returns a Rational in CRuby; monoruby doesn't have Rational, so
    // approximate with a Float. ActiveModel only compares against 0.
    if ns == 0 {
        Ok(Value::integer(0))
    } else {
        Ok(Value::float(ns as f64 / 1_000_000_000.0))
    }
}

///
/// ### Time#to_i
#[monoruby_builtin]
fn to_i(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.timestamp(),
        TimeInner::Utc(t) => t.timestamp(),
    };
    Ok(Value::integer(s))
}

///
/// ### Time#to_f
#[monoruby_builtin]
fn to_f(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let t = self_.as_time();
    let s = match t {
        TimeInner::Local(t) => t.timestamp(),
        TimeInner::Utc(t) => t.timestamp(),
    };
    Ok(Value::float(s as f64 + t.nanosecond() as f64 / 1_000_000_000.0))
}

///
/// ### Time#utc_offset
#[monoruby_builtin]
fn utc_offset(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let offs = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.offset().local_minus_utc(),
        TimeInner::Utc(_) => 0,
    };
    Ok(Value::integer(offs as _))
}

/// ### Time#-
/// - self - time -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/=2d.html]
#[monoruby_builtin]
fn sub(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_time().clone();
    let rhs_rv = lfp.arg(0);
    if let Some(rv) = rhs_rv.try_rvalue()
        && rv.ty() == ObjTy::TIME
    {
        let rhs = rhs_rv.as_time().clone();
        let res = ((lhs - rhs).num_nanoseconds().unwrap() as f64) / 1_000_000_000.0;
        Ok(Value::float(res))
    } else {
        // Time - numeric (seconds)
        let secs = rhs_rv.coerce_to_f64(vm, globals)?;
        let nanos = (secs * 1_000_000_000.0) as i64;
        let duration = chrono::Duration::nanoseconds(nanos);
        let result = lhs - duration;
        Ok(Value::new_time(result))
    }
}

///
/// ### Time#+
/// - self + other -> Time
///
/// `other` is a numeric number of seconds (integer or float). Returns a
/// new Time advanced by that many seconds. (Unlike Time#-, Time + Time is
/// not a valid Ruby operation.)
#[monoruby_builtin]
fn add(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs = self_.as_time().clone();
    let rhs_rv = lfp.arg(0);
    let secs = rhs_rv.coerce_to_f64(vm, globals)?;
    let nanos = (secs * 1_000_000_000.0) as i64;
    let duration = chrono::Duration::nanoseconds(nanos);
    let result = lhs + duration;
    Ok(Value::new_time(result))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TimeInner {
    Local(DateTime<FixedOffset>),
    Utc(DateTime<Utc>),
}

impl PartialOrd for TimeInner {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TimeInner {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Normalise both sides to UTC before comparing — `Local` and
        // `Utc` variants both back onto the same `DateTime<Tz>` /
        // `DateTime<Utc>` instants, just with a different display
        // offset. CRuby compares Time by absolute instant.
        let l = match self {
            TimeInner::Local(t) => t.with_timezone(&Utc),
            TimeInner::Utc(t) => *t,
        };
        let r = match other {
            TimeInner::Local(t) => t.with_timezone(&Utc),
            TimeInner::Utc(t) => *t,
        };
        l.cmp(&r)
    }
}

/// Compare `self` against `other` for Time#<=>, Time#<, etc.
/// Returns `None` when `other` isn't a Time (CRuby: `<=>` ⇒ nil,
/// the relational operators ⇒ ArgumentError).
fn time_cmp_opt(self_: Value, other: Value) -> Option<std::cmp::Ordering> {
    let rv = other.try_rvalue()?;
    if rv.ty() != ObjTy::TIME {
        return None;
    }
    let lhs = self_.as_time();
    let rhs = other.as_time();
    Some(lhs.cmp(rhs))
}

/// `Time#<=>` — returns -1/0/1 against another Time, nil otherwise.
#[monoruby_builtin]
fn cmp(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match time_cmp_opt(lfp.self_val(), lfp.arg(0)) {
        Some(std::cmp::Ordering::Less) => Value::integer(-1),
        Some(std::cmp::Ordering::Equal) => Value::integer(0),
        Some(std::cmp::Ordering::Greater) => Value::integer(1),
        None => Value::nil(),
    })
}

#[monoruby_builtin]
fn lt(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match time_cmp_opt(lfp.self_val(), lfp.arg(0)) {
        Some(ord) => Value::bool(ord == std::cmp::Ordering::Less),
        None => return Err(MonorubyErr::argumenterr("comparison of Time with non-Time")),
    })
}

#[monoruby_builtin]
fn le(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match time_cmp_opt(lfp.self_val(), lfp.arg(0)) {
        Some(ord) => Value::bool(ord != std::cmp::Ordering::Greater),
        None => {
            return Err(MonorubyErr::argumenterr("comparison of Time with non-Time"));
        }
    })
}

#[monoruby_builtin]
fn gt(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match time_cmp_opt(lfp.self_val(), lfp.arg(0)) {
        Some(ord) => Value::bool(ord == std::cmp::Ordering::Greater),
        None => return Err(MonorubyErr::argumenterr("comparison of Time with non-Time")),
    })
}

#[monoruby_builtin]
fn ge(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(match time_cmp_opt(lfp.self_val(), lfp.arg(0)) {
        Some(ord) => Value::bool(ord != std::cmp::Ordering::Less),
        None => {
            return Err(MonorubyErr::argumenterr("comparison of Time with non-Time"));
        }
    })
}

#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(
        time_cmp_opt(lfp.self_val(), lfp.arg(0)) == Some(std::cmp::Ordering::Equal),
    ))
}

#[monoruby_builtin]
fn eql(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // `eql?` is stricter: only true for two Times *and* equal in
    // both instant and offset. We approximate by comparing the
    // tagged enum directly, which captures both pieces.
    let other = lfp.arg(0);
    let Some(rv) = other.try_rvalue() else {
        return Ok(Value::bool(false));
    };
    if rv.ty() != ObjTy::TIME {
        return Ok(Value::bool(false));
    }
    Ok(Value::bool(
        lfp.self_val().as_time() == other.as_time(),
    ))
}

impl std::ops::Sub<Self> for TimeInner {
    type Output = Duration;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TimeInner::Local(t), TimeInner::Local(rhs)) => t - rhs,
            (TimeInner::Local(t), TimeInner::Utc(rhs)) => t.with_timezone(&Utc) - rhs,
            (TimeInner::Utc(t), TimeInner::Local(rhs)) => t - rhs.with_timezone(&Utc),
            (TimeInner::Utc(t), TimeInner::Utc(rhs)) => t - rhs,
        }
    }
}

impl std::ops::Sub<Duration> for TimeInner {
    type Output = TimeInner;
    fn sub(self, rhs: Duration) -> Self::Output {
        match self {
            TimeInner::Local(t) => TimeInner::Local(t - rhs),
            TimeInner::Utc(t) => TimeInner::Utc(t - rhs),
        }
    }
}

impl std::ops::Add<Duration> for TimeInner {
    type Output = TimeInner;
    fn add(self, rhs: Duration) -> Self::Output {
        match self {
            TimeInner::Local(t) => TimeInner::Local(t + rhs),
            TimeInner::Utc(t) => TimeInner::Utc(t + rhs),
        }
    }
}

impl std::fmt::Display for TimeInner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TimeInner::Local(t) => write!(f, "{}", t.format("%Y-%m-%d %H:%M:%S %z")),
            TimeInner::Utc(t) => write!(f, "{}", t.format("%Y-%m-%d %H:%M:%S UTC")),
        }
    }
}

impl TimeInner {
    fn utc(&mut self) {
        *self = match self {
            TimeInner::Local(t) => TimeInner::Utc((*t).into()),
            TimeInner::Utc(_) => return,
        }
    }

    fn local(&mut self) {
        *self = match self {
            TimeInner::Local(_) => return,
            TimeInner::Utc(t) => {
                let local: DateTime<Local> = (*t).into();
                TimeInner::Local(local.into())
            }
        }
    }

    /// Move to a fixed-offset zone, recomputing the local clock fields.
    /// Used by `Time#localtime(offset)`.
    fn shift_to_offset(&mut self, offset: FixedOffset) {
        *self = match self {
            TimeInner::Local(t) => TimeInner::Local(t.with_timezone(&offset)),
            TimeInner::Utc(t) => TimeInner::Local(t.with_timezone(&offset)),
        }
    }

    fn is_utc(&self) -> bool {
        match self {
            TimeInner::Local(_) => false,
            TimeInner::Utc(_) => true,
        }
    }

    pub fn year(&self) -> i32 {
        match self {
            TimeInner::Local(t) => t.year(),
            TimeInner::Utc(t) => t.year(),
        }
    }

    pub fn month(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.month(),
            TimeInner::Utc(t) => t.month(),
        }
    }

    pub fn day(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.day(),
            TimeInner::Utc(t) => t.day(),
        }
    }

    pub fn hour(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.hour(),
            TimeInner::Utc(t) => t.hour(),
        }
    }

    pub fn minute(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.minute(),
            TimeInner::Utc(t) => t.minute(),
        }
    }

    pub fn second(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.second(),
            TimeInner::Utc(t) => t.second(),
        }
    }

    pub fn nanosecond(&self) -> u32 {
        match self {
            TimeInner::Local(t) => t.nanosecond(),
            TimeInner::Utc(t) => t.nanosecond(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn test_time() {
        run_test(
            "
            before = Time.now
            (Time.now - before).class.to_s",
        );
        run_test_error("Time.local(40000000000000000000000,1,1,20,15,1,1000)");
        run_test_error("Time.local(2000,17,1,20,15,1,1000)");
        run_test_error("Time.local(2000,1,-1,20,15,1,1000)");
        run_test_error("Time.local(2000,1,1,-20,15,1,1000)");
        run_test_error("Time.local(2000,1,1,20,61,1,1000)");
        run_test_error("Time.local(2000,1,1,20,15,62,1000)");
        run_test_error("Time.local(2000,1,1,20,15,1,1000000)");
    }

    #[test]
    fn test_time2() {
        run_test(
            r#"
            res = []
            t = Time.local(2000,1,1,20,15,1,1000)     # => 2000-01-01 20:15:01.001 +0900
            res << t.gmt?                             # => false
            res << t.gmtime.to_s                      # => 2000-01-01 11:15:01 UTC
            res << t.gmt?                             # => true
            t = Time.gm(2000,1,1,20,15,1,1000)        # => 2000-01-01 20:15:01 +0900
            res << t.gmt?                             # => true
            res << t.localtime.to_s                   # => 2000-01-02 05:15:01 +0900
            res << t.gmt?                             # => false
       "#,
        );
        run_test(
            r#"
            res = []
            t = Time.local(2000,1,1,20,15,1,1000)     # => 2000-01-01 20:15:01.001 +0900
            res << t.year                             # => 2000
            res << t.month                            # => 1
            res << t.day                              # => 1
            res
       "#,
        );
        run_test(
            r#"
            res = []
            t = Time.utc(2000,1,1,20,15,1,1000)       # => 2000-01-01 20:15:01.001 +0900
            res << t.year                             # => 2000
            res << t.month                            # => 1
            res << t.day                              # => 1
            res
       "#,
        );
    }

    #[test]
    fn test_time_at() {
        run_test(
            r#"
            t = Time.at(0)
            t.utc.year
            "#,
        );
        run_test(
            r#"
            t = Time.at(946684800)
            t.utc.year
            "#,
        );
        run_test(
            r#"
            t = Time.at(1000000000, 500000)
            t.utc.to_s
            "#,
        );
    }

    #[test]
    fn time_sub() {
        // Time - Time => Float (seconds)
        run_test(
            r#"
            t1 = Time.at(1000)
            t2 = Time.at(900)
            (t1 - t2).class
            "#,
        );
        // Time - numeric => Time
        run_test(
            r#"
            t = Time.at(1000)
            (t - 100).is_a?(Time)
            "#,
        );
    }

    #[test]
    fn time_accessors() {
        // Test yday, wday, hour, min, sec
        run_test(
            r#"
            t = Time.local(2000,3,1,14,30,45)
            [t.yday, t.wday, t.hour, t.min, t.sec]
            "#,
        );
        run_test(
            r#"
            t = Time.utc(2000,1,1,0,0,0)
            [t.yday, t.wday, t.hour, t.min, t.sec]
            "#,
        );
    }

    #[test]
    fn time_usec_nsec() {
        run_test(
            r#"
            t = Time.utc(2000,1,1,20,15,1,123456)
            [t.usec, t.nsec, t.tv_nsec]
            "#,
        );
    }

    #[test]
    fn time_subsec() {
        run_test("Time.utc(2000,1,1,0,0,0).subsec");
        // CRuby returns Rational for non-zero subsec; monoruby approximates
        // with Float. Just test that the value is numeric and non-zero.
        run_test_once("Time.utc(2000,1,1,0,0,0,500000).subsec > 0");
    }

    #[test]
    fn time_to_i_to_f() {
        run_test(
            r#"
            t = Time.at(946684800)
            [t.to_i, t.tv_sec]
            "#,
        );
        run_test(
            r#"
            t = Time.at(946684800, 500000)
            t.to_f.class
            "#,
        );
    }

    #[test]
    fn time_utc_offset() {
        run_test("Time.utc(2000).utc_offset");
        run_test_once("Time.local(2000).utc_offset.is_a?(Integer)");
    }

    #[test]
    fn time_add() {
        run_test(
            r#"
            t = Time.at(1000)
            (t + 100).is_a?(Time)
            "#,
        );
        run_test(
            r#"
            t = Time.at(1000)
            (t + 100).to_i - t.to_i
            "#,
        );
        run_test(
            r#"
            t = Time.at(1000)
            (t + 0.5).to_f - t.to_f > 0
            "#,
        );
    }

    #[test]
    fn time_weekday_predicates() {
        run_test(
            r#"
            t = Time.utc(2000,1,3) # Monday
            [t.sunday?, t.monday?, t.tuesday?, t.wednesday?,
             t.thursday?, t.friday?, t.saturday?]
            "#,
        );
    }

    #[test]
    fn time_gmt_offset_aliases() {
        run_test("Time.utc(2000).gmt_offset");
        run_test("Time.utc(2000).gmtoff");
    }

    #[test]
    fn time_dst() {
        run_test("Time.utc(2000).dst?");
        run_test("Time.utc(2000).isdst");
    }

    #[test]
    fn time_zone() {
        run_test("Time.utc(2000).zone");
    }

    #[test]
    fn time_gm_string_args() {
        // String args parse as base-10 numerals, mirroring CRuby.
        run_test(r#"Time.gm("2000", "12", "1", "5", "8", "8").inspect"#);
        // Short month-name lookup ("jan" → 1, "Dec" → 12, …).
        run_test(r#"Time.gm(2000, "jan").inspect"#);
        run_test(r#"Time.gm(2000, "dec").inspect"#);
    }

    #[test]
    fn time_gm_c_style_10_args() {
        // 10-arg form: (sec, min, hour, mday, mon, year, wday, yday, isdst, tz).
        // Trailing wday/yday/isdst/tz are ignored.
        run_test(
            r#"Time.gm(1, 15, 20, 1, 1, 2000, :ignored, :ignored, :ignored, :ignored).inspect"#,
        );
        // Float-form 10-arg (numeric to_int truncation).
        run_test(
            r#"Time.gm(1.0, 15.0, 20.0, 1.0, 1.0, 2000.0, nil, nil, false, nil).inspect"#,
        );
    }

    #[test]
    fn time_gm_nil_defaults() {
        // Trailing nil month/day/etc default to 1/0.
        run_test("Time.gm(2000, nil, nil, nil, nil, nil).inspect");
    }

    #[test]
    fn time_getutc_getgm() {
        run_test("Time.local(2000,1,1,9,0,0).getutc.to_s");
        run_test("Time.local(2000,1,1,9,0,0).getgm.utc?");
        run_test("Time.utc(2000,1,1).getutc.utc?");
    }

    #[test]
    fn time_getlocal() {
        run_test_once("Time.utc(2000,1,1).getlocal.is_a?(Time)");
        run_test("Time.utc(2000,1,1,12,0,0).getlocal(3600).to_s");
        run_test("Time.utc(2000,1,1,12,0,0).getlocal(-18000).to_s");
    }

    #[test]
    fn time_to_a() {
        run_test("Time.utc(2000,1,2,3,4,5).to_a");
    }

    #[test]
    fn time_iso8601() {
        run_test(r#"Time.utc(2000,1,2,3,4,5).iso8601"#);
        run_test(r#"Time.utc(2000,1,2,3,4,5).xmlschema"#);
    }

    #[test]
    fn time_asctime_ctime() {
        run_test(r#"Time.utc(2000,1,2,3,4,5).asctime"#);
        run_test(r#"Time.utc(2000,1,2,3,4,5).ctime"#);
    }

    #[test]
    fn time_floor_ceil_round() {
        run_test(r#"Time.utc(2000,1,1,0,0,0,500000).floor.usec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,500000).ceil.usec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,500000).ceil.sec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,499999).round.usec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,500000).round.usec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,123456).floor(3).usec"#);
        run_test(r#"Time.utc(2000,1,1,0,0,0,123456).ceil(3).usec"#);
    }

    #[test]
    fn time_deconstruct_keys() {
        run_test(
            r#"
            t = Time.utc(2022, 10, 5, 13, 30)
            h = t.deconstruct_keys(nil)
            [h[:year], h[:month], h[:day], h[:hour], h[:min], h[:sec], h[:zone], h[:dst]]
            "#,
        );
        run_test(
            r#"Time.utc(2022,10,5,13,30).deconstruct_keys([:year, :month])"#,
        );
        run_test(r#"Time.utc(2022,10,5,13,30).deconstruct_keys([])"#);
        // non-symbol entries are silently dropped
        run_test(r#"Time.utc(2022,10,5,13,30).deconstruct_keys(['year', []])"#);
        // TypeError for wrong arg types
        run_test_error(r#"Time.utc(2022,10,5,13,30).deconstruct_keys(1)"#);
        run_test_error(r#"Time.utc(2022,10,5,13,30).deconstruct_keys("asd")"#);
        run_test_error(r#"Time.utc(2022,10,5,13,30).deconstruct_keys({})"#);
    }

    #[test]
    fn time_strftime_nanoseconds() {
        run_tests(&[
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%3N")"#,
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%6N")"#,
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%9N")"#,
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%N")"#,
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%L")"#,
            r#"Time.utc(2000,1,1,20,15,1,123456).strftime("%Y-%m-%d %H:%M:%S.%3N")"#,
        ]);
    }

    #[test]
    fn time_dump_load_roundtrip() {
        // The published bytestring for Time.at(946812800).gmtime is fixed
        // by ruby/spec; we match it exactly.
        run_test(
            r#"
            t = Time.at(946812800).gmtime
            t.send(:_dump).bytes
            "#,
        );
        // Round-trip a UTC Time through _dump / _load.
        run_test(
            r#"
            t = Time.utc(2000, 1, 15, 20, 1, 1, 203)
            r = Time.send(:_load, t.send(:_dump))
            [r.year, r.month, r.day, r.hour, r.min, r.sec, r.usec, r.utc?]
            "#,
        );
    }

    #[test]
    fn time_load_is_private() {
        // `Time._load(...)` from outside the class raises NoMethodError;
        // calling via `send` bypasses the private check.
        run_test_error(r#"Time._load("\x00" * 8)"#);
    }

    #[test]
    fn time_gm_carry_over() {
        // sec=60 carries to next minute (leap-second tolerance).
        run_test(
            r#"
            t = Time.gm(1972, 6, 30, 23, 59, 60)
            [t.year, t.mon, t.mday, t.hour, t.min, t.sec]
            "#,
        );
        // mday=31 in Feb carries forward (Feb has 28 days in 1999).
        run_test(r#"Time.gm(1999, 2, 31).inspect"#);
        // mday=32 still rejected (hard ceiling).
        run_test_error(r#"Time.gm(2008, 1, 32)"#);
    }

    #[test]
    fn time_gm_fractional_sec() {
        // Float fractional sec → usec.
        run_test(
            r#"
            t = Time.gm(2000, 1, 1, 20, 15, 1.75)
            [t.sec, t.usec]
            "#,
        );
        // Rational preserves nanosecond precision.
        run_test(
            r#"
            t = Time.utc(2010, 3, 30, 5, 43, "25.0123456789".to_r)
            [t.sec, t.nsec]
            "#,
        );
    }

    #[test]
    fn time_ceil_with_digits() {
        run_test(
            r#"
            t = Time.utc(2010, 3, 30, 5, 43, "25.0123456789".to_r)
            t.ceil(4).iso8601(4)
            "#,
        );
    }

    #[test]
    fn time_iso8601_year_padding() {
        // Year < 4 digits is zero-padded to 4 digits.
        run_test(r#"Time.utc(12, 4, 12).iso8601"#);
        // Year > 4 digits emits all of them.
        run_test(r#"Time.utc(40000, 4, 12).iso8601"#);
    }

    #[test]
    fn time_iso8601_with_precision() {
        run_test(r#"Time.utc(1985, 4, 12, 23, 20, 50, 521245).iso8601(2)"#);
        run_test(r#"Time.utc(1985, 4, 12, 23, 20, 50, 521245).iso8601(9)"#);
    }

    #[test]
    fn time_gmtime_frozen_no_op() {
        // Already-UTC frozen time: gmtime is a no-op.
        run_test(r#"Time.utc(2000).freeze.gmtime.utc?"#);
    }

    #[test]
    fn time_gmtime_frozen_raises() {
        // Frozen non-UTC time raises FrozenError.
        run_test_error(
            r#"
            t = Time.local(2000, 1, 1)
            t.freeze
            t.gmtime
            "#,
        );
    }

    #[test]
    fn time_new_with_args() {
        // `Time.new` previously ignored its arguments and returned the
        // current time. Now it honours the `Time.gm`-style coordinates
        // and uses the system local zone.
        run_test(r#"Time.new(2024, 6, 15, 12, 30, 45).inspect"#);
        run_test(r#"Time.new(2024).inspect"#);
        run_test(r#"Time.new(2024, "jun", 15).inspect"#);
        // String args parse as base-10 numerals.
        run_test(r#"Time.new("2024", "6", "15", "12", "30", "45").inspect"#);
    }

    #[test]
    fn time_new_with_utc_offset_string() {
        // `+HH:MM` / `+HH:MM:SS` strings give a fixed-offset zone.
        run_test(
            r#"
            t = Time.new(2013, 3, 17, nil, nil, nil, "+03:00")
            [t.utc_offset, t.year, t.mon, t.mday]
            "#,
        );
        run_test(
            r#"
            t = Time.new(2013, 3, 17, 12, 0, 0, "-05:30")
            [t.utc_offset, t.hour, t.min]
            "#,
        );
        run_test(
            r#"
            t = Time.new(2013, 3, 17, 12, 0, 0, "+09:00:00")
            t.utc_offset
            "#,
        );
    }

    #[test]
    fn time_new_with_utc_offset_integer() {
        run_test(
            r#"
            t = Time.new(2013, 3, 17, 12, 0, 0, 7245)
            t.utc_offset
            "#,
        );
        // utc_offset = nil → local zone (same as no-arg).
        run_test(
            r#"
            a = Time.new(2024, 1, 1, 0, 0, 0)
            b = Time.new(2024, 1, 1, 0, 0, 0, nil)
            a.utc_offset == b.utc_offset
            "#,
        );
    }

    #[test]
    fn time_new_invalid_utc_offset_raises() {
        // Malformed `utc_offset` string — `:any_string` etc.
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "not-an-offset")"#);
        // Out-of-range hour (24+).
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "+25:00")"#);
    }

    #[test]
    fn time_utc_offset_string_format_errors() {
        // `parse_utc_offset_string` rejects every malformed shape with
        // ArgumentError. Each case matches a distinct early-return in
        // the parser.
        for bad in [
            // No leading sign.
            r#""05:00""#,
            // Single-digit hour.
            r#""+5:00""#,
            // Wrong separator.
            r#""+05-00""#,
            // Length 7 (between `+HH:MM` and `+HH:MM:SS`).
            r#""+05:00:1""#,
            // Double sign.
            r#""++05:00""#,
            r#""--00:00""#,
            // Just a sign.
            r#""+""#,
            r#""-""#,
            // Hour digits non-numeric.
            r#""+0a:00""#,
            // Minute digits non-numeric.
            r#""+05:M0""#,
            // Second digits non-numeric (in `+HH:MM:SS` form).
            r#""+05:00:0X""#,
            // Wrong second-separator.
            r#""+05:00-30""#,
            // Empty string.
            r#""""#,
        ] {
            let code = format!(
                "Time.new(2024, 1, 1, 0, 0, 0, {})",
                bad
            );
            run_test_error(&code);
        }
    }

    #[test]
    fn time_utc_offset_range_errors() {
        // Hour, minute, second each enforce per-field ranges.
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "+24:00")"#);
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "-25:00")"#);
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "+05:60")"#);
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "+05:00:60")"#);
    }

    #[test]
    fn time_utc_offset_non_ascii_raises() {
        // Non-ASCII bytes in the offset string are rejected before parse.
        run_test_error(r#"Time.new(2024, 1, 1, 0, 0, 0, "\xff05:00")"#);
    }

    #[test]
    fn time_gm_field_range_errors() {
        // mon / mday lower-bound is 1.
        run_test_error(r#"Time.gm(2008, 0, 1)"#);
        run_test_error(r#"Time.gm(2008, 1, 0)"#);
        // mon upper-bound is 12.
        run_test_error(r#"Time.gm(2008, 13, 1)"#);
        // min / sec upper bounds.
        run_test_error(r#"Time.gm(2008, 1, 1, 0, 60)"#);
        run_test_error(r#"Time.gm(2008, 1, 1, 0, 0, 61)"#);
        // Negative sec.
        run_test_error(r#"Time.gm(2008, 1, 1, 0, 0, -1)"#);
    }

    #[test]
    fn time_gm_invalid_string_year_raises() {
        // Non-numeric String → ArgumentError, not silent truncation.
        run_test_error(r#"Time.gm("twenty-twenty-four")"#);
    }

    #[test]
    fn time_gm_unknown_month_name_raises() {
        // `time_month_to_i64` falls through to numeric parse; an
        // unknown 3-letter name doesn't match either branch.
        run_test_error(r#"Time.gm(2024, "xyz", 1)"#);
    }

    #[test]
    fn time_gm_arity_8_raises() {
        // Slot 7 is the C-style trigger only; counts 8/9 fall through
        // to the standard "wrong number of arguments" path.
        run_test_error(r#"Time.gm(2024, 1, 1, 0, 0, 0, 0, 0)"#);
    }

    #[test]
    fn time_localtime_invalid_offset_raises() {
        // String-form errors propagate from `parse_utc_offset_string`.
        run_test_error(
            r#"
            t = Time.utc(2024, 6, 15, 12, 0, 0)
            t.localtime("not-an-offset")
            "#,
        );
        // Out-of-range integer offset.
        run_test_error(
            r#"
            t = Time.utc(2024, 6, 15, 12, 0, 0)
            t.localtime(99999999)
            "#,
        );
    }

    #[test]
    fn time_localtime_frozen_with_different_offset_raises() {
        // Frozen Time + offset that differs from current zone →
        // FrozenError (matches CRuby's check ordering: zone-equality
        // short-circuit first, then frozen guard).
        run_test_error(
            r#"
            t = Time.utc(2024, 6, 15, 12, 0, 0)
            t.freeze
            t.localtime("+05:00")
            "#,
        );
    }

    #[test]
    fn time_getlocal_invalid_offset_raises() {
        run_test_error(
            r#"
            Time.utc(2024).getlocal("garbage")
            "#,
        );
        run_test_error(
            r#"
            Time.utc(2024).getlocal("+99:00")
            "#,
        );
    }
}
