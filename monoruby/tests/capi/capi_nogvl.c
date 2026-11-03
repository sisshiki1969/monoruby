/*
 * rb_thread_call_without_gvl's unblock function: Thread#wakeup and a
 * trapped signal run it (a pipe write here), and RUBY_UBF_IO lets the
 * interrupt cut a blocking read short with EINTR. Built against CRuby
 * and against monoruby's layer; tests/capi.rs compares what
 * capi_nogvl.rb prints.
 */
#include <ruby.h>
#include <ruby/thread.h>
#include <errno.h>
#include <unistd.h>

/* Blocks until the unblock function writes 'A' (EINTR is retried, so a
   signal alone never ends it). */
static void *
blocking_read(void *data)
{
    int rfd = *(int *)data;
    char c = ' ';
    ssize_t r;
    do {
        r = read(rfd, &c, 1);
    } while (r == -1 && errno == EINTR);
    close(rfd);
    return (void *)((r == 1 && c == 'A') ? Qtrue : Qfalse);
}

static void
unblock_write(void *data)
{
    int wfd = *(int *)data;
    char c = 'A';
    ssize_t r;
    do {
        r = write(wfd, &c, 1);
    } while (r == -1 && errno == EINTR);
    close(wfd);
}

/* N.with_ubf: true when the unblock function ended the wait */
static VALUE
n_with_ubf(VALUE self)
{
    int fds[2];
    (void)self;
    if (pipe(fds) == -1)
        rb_sys_fail("pipe");
    return (VALUE)rb_thread_call_without_gvl(blocking_read, &fds[0], unblock_write, &fds[1]);
}

/* A single read: true when it was interrupted (EINTR). */
static void *
read_once(void *data)
{
    int rfd = (int)(size_t)data;
    char c;
    return (void *)((read(rfd, &c, 1) == -1 && errno == EINTR) ? Qtrue : Qfalse);
}

/* N.with_ubf_io: true when the interrupt cut the read short */
static VALUE
n_with_ubf_io(VALUE self)
{
    int fds[2];
    VALUE ret;
    (void)self;
    if (pipe(fds) == -1)
        rb_sys_fail("pipe");
    ret = (VALUE)rb_thread_call_without_gvl(read_once, (void *)(size_t)fds[0], RUBY_UBF_IO, 0);
    close(fds[0]);
    close(fds[1]);
    return ret;
}

/* N.plain(x): no unblock function at all; runs and returns x */
static void *
identity(void *data)
{
    return data;
}

static VALUE
n_plain(VALUE self, VALUE x)
{
    (void)self;
    return (VALUE)rb_thread_call_without_gvl(identity, (void *)x, NULL, NULL);
}

void
Init_capi_nogvl(void)
{
    VALUE m = rb_define_module("CapiNogvl");
    rb_define_module_function(m, "with_ubf", n_with_ubf, 0);
    rb_define_module_function(m, "with_ubf_io", n_with_ubf_io, 0);
    rb_define_module_function(m, "plain", n_plain, 1);
}
