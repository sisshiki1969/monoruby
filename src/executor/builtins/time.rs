use crate::*;

use chrono::{DateTime, Duration, FixedOffset, Utc};

//
// Time class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(TIME_CLASS, "new", now, 0);
    globals.define_builtin_singleton_func(TIME_CLASS, "now", now, 0);
    globals.define_builtin_func(TIME_CLASS, "-", sub, 1);
}

/// ### Time.new
/// - new -> Time
/// - now -> Time
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/s/new.html]
extern "C" fn now(
    _vm: &mut Executor,
    _globals: &mut Globals,
    _self_val: Value,
    _arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let t = Utc::now().with_timezone(&FixedOffset::east_opt(9 * 3600).unwrap());
    let time_info = TimeInfo::Local(t);
    Some(Value::new_time(time_info))
}

/// ### Time#-
/// - self - time -> Float
///
/// [https://docs.ruby-lang.org/ja/latest/method/Time/i/=2d.html]
extern "C" fn sub(
    _vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    _len: usize,
    _: Option<BlockHandler>,
) -> Option<Value> {
    let lhs_rv = self_val.try_rvalue().unwrap();
    let lhs = match lhs_rv.kind() {
        ObjKind::TIME => lhs_rv.as_time().clone(),
        _ => unreachable!(),
    };
    let rhs_rv = arg[0].try_rvalue().unwrap();
    let rhs = match rhs_rv.kind() {
        ObjKind::TIME => rhs_rv.as_time().clone(),
        _ => {
            globals.err_method_not_found(IdentId::_SUB, self_val);
            return None;
        }
    };
    let res = ((lhs - rhs).num_nanoseconds().unwrap() as f64) / 1000.0 / 1000.0 / 1000.0;
    Some(Value::new_float(res))
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TimeInfo {
    Local(DateTime<FixedOffset>),
    UTC(DateTime<Utc>),
}

impl std::ops::Sub<Self> for TimeInfo {
    type Output = Duration;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (TimeInfo::Local(t), TimeInfo::Local(rhs)) => t - rhs,
            (TimeInfo::Local(t), TimeInfo::UTC(rhs)) => t.with_timezone(&Utc) - rhs,
            (TimeInfo::UTC(t), TimeInfo::Local(rhs)) => t - rhs.with_timezone(&Utc),
            (TimeInfo::UTC(t), TimeInfo::UTC(rhs)) => t - rhs,
        }
    }
}

impl std::fmt::Display for TimeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TimeInfo::Local(t) => write!(f, "{}", t.format("%F %T %z")),
            TimeInfo::UTC(t) => write!(f, "{}", t.format("%F %T %z")),
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
