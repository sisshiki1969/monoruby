use crate::*;

use chrono::{DateTime, Duration, FixedOffset, Utc};

//
// Time class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(TIME_CLASS, "new", now, 0);
    globals.define_builtin_class_func(TIME_CLASS, "now", now, 0);
    globals.define_builtin_func(TIME_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(TIME_CLASS, "to_s", to_s, 0);
    globals.define_builtin_func(TIME_CLASS, "strftime", strftime, 1);
    globals.define_builtin_func(TIME_CLASS, "-", sub, 1);
}

///
/// ### Time.new
/// - new -> Time
/// - now -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/new.html]
fn now(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let t = Utc::now().with_timezone(&FixedOffset::east_opt(9 * 3600).unwrap());
    let time_info = TimeInner::Local(t);
    Ok(Value::new_time(time_info))
}

///
/// ### Time#inspect
/// - inspect -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/inspect.html]
fn inspect(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let time = lfp.self_val().as_time().to_string();
    Ok(Value::new_string(time))
}

///
/// ### Time#to_s
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/to_s.html]
fn to_s(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format("%Y-%m-%d %H:%M:%S %z"),
        TimeInner::UTC(t) => t.format("%Y-%m-%d %H:%M:%S UTC"),
    }
    .to_string();
    Ok(Value::new_string(s))
}

///
/// ### Time#strftime
/// - strftime(format) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/strftime.html]
fn strftime(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _: usize,
) -> Result<Value> {
    let mut fmt = arg[0].expect_string(globals)?;
    fmt = fmt.replace("%N", "%f");
    let s = match lfp.self_val().as_time() {
        TimeInner::Local(t) => t.format(&fmt).to_string(),
        TimeInner::UTC(t) => {
            let fmt = fmt + " UTC";
            t.format(&fmt).to_string()
        }
    };
    Ok(Value::new_string(s))
}

/// ### Time#-
/// - self - time -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/=2d.html]
fn sub(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    arg: Arg,
    _len: usize,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let lhs_rv = self_.try_rvalue().unwrap();
    let lhs = match lhs_rv.kind() {
        ObjKind::TIME => lhs_rv.as_time().clone(),
        _ => unreachable!(),
    };
    let rhs_rv = arg[0].try_rvalue().unwrap();
    let rhs = match rhs_rv.kind() {
        ObjKind::TIME => rhs_rv.as_time().clone(),
        _ => {
            return Err(MonorubyErr::method_not_found(globals, IdentId::_SUB, self_));
        }
    };
    let res = ((lhs - rhs).num_nanoseconds().unwrap() as f64) / 1000.0 / 1000.0 / 1000.0;
    Ok(Value::new_float(res))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TimeInner {
    Local(DateTime<FixedOffset>),
    UTC(DateTime<Utc>),
}

impl std::ops::Sub<Self> for TimeInner {
    type Output = Duration;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TimeInner::Local(t), TimeInner::Local(rhs)) => t - rhs,
            (TimeInner::Local(t), TimeInner::UTC(rhs)) => t.with_timezone(&Utc) - rhs,
            (TimeInner::UTC(t), TimeInner::Local(rhs)) => t - rhs.with_timezone(&Utc),
            (TimeInner::UTC(t), TimeInner::UTC(rhs)) => t - rhs,
        }
    }
}

impl std::fmt::Display for TimeInner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TimeInner::Local(t) => write!(f, "{}", t.format("%Y-%m-%d %H:%M:%S.%f %z")),
            TimeInner::UTC(t) => write!(f, "{}", t.format("%Y-%m-%d %H:%M:%S.%f UTC")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_time() {
        run_test(
            "
            before = Time.now
            (Time.now - before).class.to_s",
        );
    }
}
