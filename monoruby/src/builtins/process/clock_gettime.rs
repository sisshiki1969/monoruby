#[derive(Debug, Clone, Default)]
pub(super) struct TimeSpec {
    pub tv_sec: i64,
    pub tv_nsec: i64,
}

impl TimeSpec {
    pub fn sec(&self) -> i64 {
        self.tv_sec + self.tv_nsec / 1_000_000_000
    }

    pub fn millisec(&self) -> i64 {
        self.tv_sec * 1_000 + self.tv_nsec / 1_000_000
    }

    pub fn microsec(&self) -> i64 {
        self.tv_sec * 1_000_000 + self.tv_nsec / 1_000
    }

    pub fn nanosec(&self) -> i64 {
        self.tv_sec * 1_000_000_000 + self.tv_nsec
    }
}

pub(super) fn clock_gettime(clk_id: i32, tp: &mut TimeSpec) -> Result<(), i32> {
    unsafe {
        let res = libc::clock_gettime(clk_id, tp as *mut _ as *mut libc::timespec);
        if res == 0 {
            Ok(())
        } else {
            Err(*libc::__errno_location())
        }
    }
}
