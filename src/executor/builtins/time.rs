use crate::*;

use chrono::{DateTime, Duration, FixedOffset, Utc};

//
// Time class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(TIME_CLASS, "now", now, 0);
    globals.define_builtin_func(TIME_CLASS, "-", sub, 1);
}

extern "C" fn now(
    _vm: &mut Interp,
    _globals: &mut Globals,
    _arg: Arg,
    _len: usize,
) -> Option<Value> {
    let t = Utc::now().with_timezone(&FixedOffset::east(9 * 3600));
    let time_info = TimeInfo::Local(t);
    Some(Value::new_time(time_info))
}

extern "C" fn sub(_vm: &mut Interp, globals: &mut Globals, arg: Arg, _len: usize) -> Option<Value> {
    let lhs = match &arg.self_value().as_rvalue().unwrap().kind {
        ObjKind::Time(time) => time.clone(),
        _ => unreachable!(),
    };
    let rhs = match &arg[0].as_rvalue().unwrap().kind {
        ObjKind::Time(time) => time.clone(),
        _ => {
            globals.err_method_not_found(IdentId::_SUB);
            return None;
        }
    };
    let res = ((lhs - rhs).num_nanoseconds().unwrap() as f64) / 1000.0 / 1000.0 / 1000.0;
    Some(Value::new_float(res))
}

#[derive(Clone, Debug, PartialEq)]
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
