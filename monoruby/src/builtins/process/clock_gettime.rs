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
    // `clockid_t` is `i32` on Linux and `u32` on macOS; `as _` defers to
    // the libc signature. errno is read via `std::io::Error::last_os_error`
    // rather than `__errno_location` (glibc) / `__error` (macOS) so we
    // don't have to fork on platform.
    //
    // monoruby's startup.rb hard-codes the Linux numeric values for
    // `Process::CLOCK_*`, so on macOS we have to translate them to the
    // host's clockid_t before the libc call. Without this, anything
    // calling `Process.clock_gettime(Process::CLOCK_MONOTONIC)` (which
    // benchmark-driver and bin/bench do) gets EINVAL because Linux
    // CLOCK_MONOTONIC=1 isn't a valid clockid on macOS (where MONOTONIC
    // is 6). Unknown ids pass through unchanged so they still surface
    // as EINVAL.
    let clk_id = translate_clk_id(clk_id);
    unsafe {
        let res = libc::clock_gettime(clk_id as _, tp as *mut _ as *mut libc::timespec);
        if res == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error().raw_os_error().unwrap_or(0))
        }
    }
}

#[cfg(target_os = "macos")]
fn translate_clk_id(linux_id: i32) -> i32 {
    // Linux clockid_t → macOS clockid_t. The Linux numbers come from
    // `Process::CLOCK_*` defined in startup.rb. macOS lacks
    // `*_COARSE` / `BOOTTIME` / `*_ALARM` flavours, so we fall back to
    // the nearest semantic equivalent (the coarse approximations match
    // CRuby-on-macOS behaviour, which silently substitutes too).
    match linux_id {
        0 => libc::CLOCK_REALTIME as i32,
        1 => libc::CLOCK_MONOTONIC as i32,
        2 => libc::CLOCK_PROCESS_CPUTIME_ID as i32,
        3 => libc::CLOCK_THREAD_CPUTIME_ID as i32,
        4 => libc::CLOCK_MONOTONIC_RAW as i32,
        5 => libc::CLOCK_MONOTONIC_RAW_APPROX as i32,
        6 => libc::CLOCK_MONOTONIC as i32,
        7 | 8 | 9 => libc::CLOCK_UPTIME_RAW as i32,
        other => other,
    }
}

#[cfg(not(target_os = "macos"))]
#[inline]
fn translate_clk_id(linux_id: i32) -> i32 {
    linux_id
}
