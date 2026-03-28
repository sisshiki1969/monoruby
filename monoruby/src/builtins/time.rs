use super::*;
use chrono::{
    DateTime, Datelike, Duration, FixedOffset, Local, LocalResult, NaiveDate, NaiveDateTime,
    NaiveTime, TimeZone, Utc,
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
    globals.define_builtin_class_func(TIME_CLASS, "allocate", allocate, 0);

    globals.define_builtin_funcs(TIME_CLASS, "gmtime", &["utc"], gmtime, 0);
    globals.define_builtin_funcs(TIME_CLASS, "gmt?", &["utc?"], gmt_, 0);
    globals.define_builtin_func(TIME_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func_with(TIME_CLASS, "localtime", localtime, 0, 1, false);
    globals.define_builtin_func(TIME_CLASS, "strftime", strftime, 1);
    globals.define_builtin_func(TIME_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(TIME_CLASS, "year", year, 0);
    globals.define_builtin_funcs(TIME_CLASS, "month", &["mon"], month, 0);
    globals.define_builtin_funcs(TIME_CLASS, "day", &["mday"], day, 0);
    globals.define_builtin_func(TIME_CLASS, "-", sub, 1);
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
        let s = secs_val.coerce_to_int(vm, globals)?;
        (s, 0u32)
    };
    let usec_ns = if let Some(arg1) = lfp.try_arg(1) {
        let u = arg1.coerce_to_int(vm, globals)?;
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

/// ### Time.allocate
#[monoruby_builtin]
fn allocate(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let time_info = TimeInner::Utc(DateTime::<Utc>::default());
    Ok(Value::new_time_with_class(time_info, class_id))
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
    let year = if let Ok(i) = i32::try_from(lfp.arg(0).coerce_to_int(vm, globals)?) {
        i
    } else {
        return Ok(None);
    };
    let mon = if let Some(mon) = lfp.try_arg(1) {
        let i = mon.coerce_to_int(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        1
    };
    let day = if let Some(day) = lfp.try_arg(2) {
        let i = day.coerce_to_int(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        1
    };
    let hour = if let Some(hour) = lfp.try_arg(3) {
        let i = hour.coerce_to_int(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let min = if let Some(min) = lfp.try_arg(4) {
        let i = min.coerce_to_int(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let sec = if let Some(sec) = lfp.try_arg(5) {
        let i = sec.coerce_to_int(vm, globals)?;
        if let Ok(i) = u32::try_from(i) {
            i
        } else {
            return Ok(None);
        }
    } else {
        0
    };
    let usec = if let Some(usec) = lfp.try_arg(6) {
        let i = usec.coerce_to_int(vm, globals)?;
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
    let fmt = lfp.arg(0).coerce_to_str(vm, globals)?.replace("%N", "%f");
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format(&fmt).to_string(),
        TimeInner::Utc(t) => {
            let fmt = fmt + " UTC";
            t.format(&fmt).to_string()
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
        run_test_once(
            r#"
            t = Time.at(1000000000, 500000)
            t.utc.to_s
            "#,
        );
    }

    #[test]
    fn time_sub() {
        // Time - Time => Float (seconds)
        run_test_once(
            r#"
            t1 = Time.at(1000)
            t2 = Time.at(900)
            (t1 - t2).class
            "#,
        );
        // Time - numeric => Time
        run_test_once(
            r#"
            t = Time.at(1000)
            (t - 100).is_a?(Time)
            "#,
        );
    }
}
