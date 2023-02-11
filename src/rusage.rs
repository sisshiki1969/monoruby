//
// getrusage
//

/// wrapper for libc::timeval
#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct Timeval {
    pub sec: i64,
    pub usec: i64,
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

pub fn rusage(who: RusageWho, usage: &mut Rusage) {
    unsafe {
        let who = match who {
            RusageWho::Self_ => libc::RUSAGE_SELF,
            RusageWho::Children => libc::RUSAGE_CHILDREN,
            RusageWho::Thread => libc::RUSAGE_THREAD,
        };
        let res = libc::getrusage(who as libc::c_int, usage as *mut _ as *mut libc::rusage);
        assert!(res == 0);
    }
}
