use crate::*;

use chrono::{DateTime, Duration, FixedOffset, Utc};

//
// Time class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(TIME_CLASS, "now", now, 0);
}

extern "C" fn now(_vm: &mut Interp, _globals: &mut Globals, _arg: Arg, _len: usize) -> Value {
    let t = Utc::now().with_timezone(&FixedOffset::east(9 * 3600));
    let time_info = TimeInfo::Local(t);
    Value::new_time(time_info)
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
