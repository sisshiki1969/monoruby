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
        7,
        false,
    );
    globals.define_builtin_class_funcs_with(TIME_CLASS, "gm", &["utc"], time_gm, 1, 7, false);
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
    globals.define_builtin_funcs(TIME_CLASS, "iso8601", &["xmlschema"], iso8601, 0);
    globals.define_builtin_func(TIME_CLASS, "asctime", asctime, 0);
    globals.define_builtin_func(TIME_CLASS, "ctime", asctime, 0);
    globals.define_builtin_func_with(TIME_CLASS, "floor", floor_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "ceil", ceil_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "round", round_, 0, 1, false);
    globals.define_builtin_func_with(TIME_CLASS, "deconstruct_keys", deconstruct_keys_, 1, 1, false);
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
    let is_utc = self_.as_time().is_utc();
    let (year, month, day, yday, wday, hour, min, sec, subsec_ns) = match self_.as_time() {
        TimeInner::Local(t) => (
            t.year(), t.month(), t.day(), t.ordinal(),
            t.weekday().num_days_from_sunday(), t.hour(), t.minute(), t.second(),
            t.nanosecond(),
        ),
        TimeInner::Utc(t) => (
            t.year(), t.month(), t.day(), t.ordinal(),
            t.weekday().num_days_from_sunday(), t.hour(), t.minute(), t.second(),
            t.nanosecond(),
        ),
    };
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
        let offset_secs = arg0.coerce_to_int_i64(vm, globals)? as i32;
        let fixed = FixedOffset::east_opt(offset_secs)
            .ok_or_else(|| MonorubyErr::argumenterr("utc_offset out of range"))?;
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
    let t = lfp.self_val();
    let is_utc = t.as_time().is_utc();
    let (sec, min, hour, day, mon, year, wday, yday) = match t.as_time() {
        TimeInner::Local(t) => (
            t.second(), t.minute(), t.hour(), t.day(), t.month(), t.year(),
            t.weekday().num_days_from_sunday(), t.ordinal(),
        ),
        TimeInner::Utc(t) => (
            t.second(), t.minute(), t.hour(), t.day(), t.month(), t.year(),
            t.weekday().num_days_from_sunday(), t.ordinal(),
        ),
    };
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
/// ### Time#iso8601
///
/// - iso8601 -> String
/// - xmlschema -> String
///
/// Returns an ISO 8601 / XML Schema representation such as
/// `"2000-01-02T03:04:05+09:00"` or `"2000-01-02T03:04:05Z"`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/iso8601.html]
#[monoruby_builtin]
fn iso8601(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format("%Y-%m-%dT%H:%M:%S%:z").to_string(),
        TimeInner::Utc(t) => t.format("%Y-%m-%dT%H:%M:%SZ").to_string(),
    };
    Ok(Value::string(s))
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
    match lfp.self_val().as_time() {
        TimeInner::Local(t) => {
            let ns = t.nanosecond();
            let new_ns = rescale_nsec(ns, precision, mode);
            let mut result = t.with_nanosecond(0).unwrap();
            if new_ns >= 1_000_000_000 {
                result = result + Duration::seconds(1);
            } else {
                result = result.with_nanosecond(new_ns).unwrap();
            }
            TimeInner::Local(result)
        }
        TimeInner::Utc(t) => {
            let ns = t.nanosecond();
            let new_ns = rescale_nsec(ns, precision, mode);
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
/// ### Time.new
/// - new -> Time
/// - now -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/new.html]
#[monoruby_builtin]
fn time_now(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let time_info = TimeInner::Local(Local::now().into());
    Ok(Value::new_time(time_info))
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
    let year = if let Ok(i) = i32::try_from(lfp.arg(0).coerce_to_int_i64(vm, globals)?) {
        i
    } else {
        return Ok(None);
    };
    let mon = if let Some(mon) = lfp.try_arg(1) {
        let i = mon.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        1
    };
    let day = if let Some(day) = lfp.try_arg(2) {
        let i = day.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        1
    };
    let hour = if let Some(hour) = lfp.try_arg(3) {
        let i = hour.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let min = if let Some(min) = lfp.try_arg(4) {
        let i = min.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let sec = if let Some(sec) = lfp.try_arg(5) {
        let i = sec.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let usec = if let Some(usec) = lfp.try_arg(6) {
        let i = usec.coerce_to_int_i64(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    Ok(Some(NaiveDateTime::new(
        NaiveDate::from_ymd_opt(year, mon, day)
            .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?,
        NaiveTime::from_hms_micro_opt(hour, min, sec, usec)
            .ok_or_else(|| MonorubyErr::argumenterr("argument out of range."))?,
    )))
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
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/gmtime.html]
#[monoruby_builtin]
fn gmtime(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().as_time_mut().utc();
    Ok(lfp.self_val())
}

///
/// ### Time#localtime
/// - localtime -> self
/// - [NOT SUPPORTED] localtime(utc_offset) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/localtime.html]
#[monoruby_builtin]
fn localtime(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    lfp.self_val().as_time_mut().local();
    Ok(lfp.self_val())
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
    let nanos = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.timestamp_subsec_nanos(),
        TimeInner::Utc(t) => t.timestamp_subsec_nanos(),
    };

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
    let year = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.year(),
        TimeInner::Utc(t) => t.year(),
    };
    Ok(Value::integer(year as _))
}

///
/// ### Time#month
/// - mon -> Integer
/// - month -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/mon.html]
#[monoruby_builtin]
fn month(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let month = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.month(),
        TimeInner::Utc(t) => t.month(),
    };
    Ok(Value::integer(month as _))
}

///
/// ### Time#day
/// - mday -> Integer
/// - day -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/day.html]
#[monoruby_builtin]
fn day(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let day = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.day(),
        TimeInner::Utc(t) => t.day(),
    };
    Ok(Value::integer(day as _))
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
    let h = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.hour(),
        TimeInner::Utc(t) => t.hour(),
    };
    Ok(Value::integer(h as _))
}

///
/// ### Time#min
#[monoruby_builtin]
fn min_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let m = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.minute(),
        TimeInner::Utc(t) => t.minute(),
    };
    Ok(Value::integer(m as _))
}

///
/// ### Time#sec
#[monoruby_builtin]
fn sec_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.second(),
        TimeInner::Utc(t) => t.second(),
    };
    Ok(Value::integer(s as _))
}

///
/// ### Time#usec
#[monoruby_builtin]
fn usec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.nanosecond(),
        TimeInner::Utc(t) => t.nanosecond(),
    };
    Ok(Value::integer((ns / 1_000) as _))
}

///
/// ### Time#nsec
#[monoruby_builtin]
fn nsec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.nanosecond(),
        TimeInner::Utc(t) => t.nanosecond(),
    };
    Ok(Value::integer(ns as _))
}

///
/// ### Time#subsec
#[monoruby_builtin]
fn subsec(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let ns = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.nanosecond(),
        TimeInner::Utc(t) => t.nanosecond(),
    };
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
    let (s, ns) = match lfp.self_val().as_time() {
        TimeInner::Local(t) => (t.timestamp(), t.nanosecond()),
        TimeInner::Utc(t) => (t.timestamp(), t.nanosecond()),
    };
    Ok(Value::float(s as f64 + ns as f64 / 1_000_000_000.0))
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

    fn is_utc(&self) -> bool {
        match self {
            TimeInner::Local(_) => false,
            TimeInner::Utc(_) => true,
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
}
