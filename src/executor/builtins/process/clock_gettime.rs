use num::ToPrimitive;

#[derive(Debug, Clone, Default)]
#[repr(C)]
pub struct TimeSpec {
    pub tv_sec: i64,
    pub tv_nsec: i64,
}

impl TimeSpec {
    pub fn to_f64(&self) -> f64 {
        self.tv_sec.to_f64().unwrap() + self.tv_nsec.to_f64().unwrap() / 1e9
    }
}

pub fn clock_gettime(clk_id: i32, tp: &mut TimeSpec) {
    unsafe {
        let res = libc::clock_gettime(clk_id, tp as *mut _ as *mut libc::timespec);
        assert!(res == 0);
    }
}
