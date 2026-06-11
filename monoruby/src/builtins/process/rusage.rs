//
// getrusage
//

use num::ToPrimitive;

/// wrapper for libc::timeval
#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct Timeval {
    pub sec: i64,
    pub usec: i64,
}

impl Timeval {
    pub fn get_f64(&self) -> f64 {
        self.sec.to_f64().unwrap() + self.usec.to_f64().unwrap() / 1e6
    }
}

/// wrapper for libc::rusage
#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct Rusage {
    pub ru_utime: Timeval,
    pub ru_stime: Timeval,
    ru_maxrss: i64,
    ru_ixrss: i64,
    ru_idrss: i64,
    ru_isrss: i64,
    ru_minflt: i64,
    ru_majflt: i64,
    ru_nswap: i64,
    ru_inblock: i64,
    ru_oublock: i64,
    ru_msgsnd: i64,
    ru_msgrcv: i64,
    ru_nsignals: i64,
    ru_nvcsw: i64,
    ru_nivcsw: i64,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum RusageWho {
    Self_,
    Children,
    Thread,
}

pub fn getrusage(who: RusageWho, usage: &mut Rusage) {
    // `RUSAGE_THREAD` is a Linux extension (no equivalent on macOS /
    // BSDs). On non-Linux hosts we fall back to `RUSAGE_SELF` so the
    // file compiles for parser / bytecodegen work; this only matters
    // when `Process.times`-style code is exercised on a non-Linux build,
    // which monoruby doesn't officially support anyway.
    unsafe {
        let who = match who {
            RusageWho::Self_ => libc::RUSAGE_SELF,
            RusageWho::Children => libc::RUSAGE_CHILDREN,
            #[cfg(target_os = "linux")]
            RusageWho::Thread => libc::RUSAGE_THREAD,
            #[cfg(not(target_os = "linux"))]
            RusageWho::Thread => libc::RUSAGE_SELF,
        };
        let res = libc::getrusage(who as libc::c_int, usage as *mut _ as *mut libc::rusage);
        assert!(res == 0);
    }
}
