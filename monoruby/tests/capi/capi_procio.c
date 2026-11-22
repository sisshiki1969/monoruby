/* The proc / fiber / thread / exception / IO / GC / process functions
 * of doc/c_api.md §3.28, driven by capi_procio.rb on CRuby and on
 * monoruby. */
#include "ruby.h"
#include "ruby/io.h"
#include "ruby/encoding.h"
#include "ruby/thread.h"
#include "ruby/util.h"
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>

/* CRuby exports these but declares them only in its internal headers. */
VALUE rb_gc_disable_no_rest(void);
VALUE rb_gcd(VALUE x, VALUE y);
VALUE rb_gcd_normal(VALUE x, VALUE y);

static VALUE m;

/* Proc / Method */

static VALUE
proc_call_kw(VALUE self, VALUE proc, VALUE args, VALUE kw)
{
    return rb_proc_call_kw(proc, args, RTEST(kw) ? RB_PASS_KEYWORDS : RB_NO_KEYWORDS);
}

static VALUE
proc_call_with_block_kw(VALUE self, VALUE proc, VALUE args, VALUE blk)
{
    return rb_proc_call_with_block_kw(proc, (int)RARRAY_LEN(args), RARRAY_CONST_PTR(args), blk, RB_PASS_KEYWORDS);
}

static VALUE
method_calls(VALUE self, VALUE meth, VALUE args, VALUE blk)
{
    int n = (int)RARRAY_LEN(args);
    const VALUE *a = RARRAY_CONST_PTR(args);
    return rb_ary_new_from_args(4, rb_method_call(n, a, meth), rb_method_call_kw(n, a, meth, RB_PASS_KEYWORDS),
                                rb_method_call_with_block(n, a, meth, blk),
                                rb_method_call_with_block_kw(n, a, meth, blk, RB_PASS_KEYWORDS));
}

static VALUE
yield_kw(VALUE self, VALUE args)
{
    VALUE a = rb_yield_values_kw((int)RARRAY_LEN(args), RARRAY_CONST_PTR(args), RB_PASS_KEYWORDS);
    VALUE b = rb_yield_splat_kw(args, RB_PASS_KEYWORDS);
    VALUE c = rb_yield_values_kw((int)RARRAY_LEN(args), RARRAY_CONST_PTR(args), RB_NO_KEYWORDS);
    return rb_ary_new_from_args(3, a, b, c);
}

static VALUE
yield_block_each(VALUE self, VALUE obj)
{
    return rb_block_call(obj, rb_intern("each"), 0, NULL, rb_yield_block, Qnil);
}

static VALUE
public_kw(VALUE self, VALUE recv, VALUE mid, VALUE args)
{
    return rb_funcallv_public_kw(recv, SYM2ID(mid), (int)RARRAY_LEN(args), RARRAY_CONST_PTR(args), RB_PASS_KEYWORDS);
}

static VALUE
eval_cmd(VALUE self, VALUE cmd, VALUE args)
{
    return rb_eval_cmd_kw(cmd, args, RB_NO_KEYWORDS);
}

static VALUE
basic_p(VALUE self, VALUE klass, VALUE mid)
{
    return rb_method_basic_definition_p(klass, SYM2ID(mid)) ? Qtrue : Qfalse;
}

/* Fiber */

static VALUE
fiber_body(RB_BLOCK_CALL_FUNC_ARGLIST(first, data))
{
    VALUE got = rb_fiber_yield(1, &first);
    VALUE pair[2] = {got, data};
    VALUE again = rb_fiber_yield(2, pair);
    return rb_ary_new_from_args(2, ID2SYM(rb_intern("done")), again);
}

static VALUE
fibers(VALUE self)
{
    VALUE f = rb_fiber_new(fiber_body, rb_str_new_cstr("data"));
    VALUE out = rb_ary_new();
    VALUE one = INT2FIX(1), two = INT2FIX(2), three = INT2FIX(3);
    rb_ary_push(out, rb_fiber_alive_p(f));
    rb_ary_push(out, rb_fiber_resume(f, 1, &one));
    rb_ary_push(out, rb_fiber_resume(f, 1, &two));
    rb_ary_push(out, rb_fiber_resume(f, 1, &three));
    rb_ary_push(out, rb_fiber_alive_p(f));
    rb_ary_push(out, rb_obj_is_kind_of(rb_fiber_current(), rb_const_get(rb_cObject, rb_intern("Fiber"))));
    return out;
}

static VALUE
fiber_raise(VALUE self)
{
    VALUE f = rb_fiber_new(fiber_body, Qnil);
    VALUE one = INT2FIX(1);
    rb_fiber_resume(f, 1, &one);
    VALUE args[2] = {rb_eArgError, rb_str_new_cstr("from C")};
    return rb_fiber_raise(f, 2, args);
}

static VALUE
fiber_storage(VALUE self, VALUE storage)
{
    VALUE f = rb_fiber_new_storage(fiber_body, Qnil, storage);
    VALUE one = INT2FIX(1);
    return rb_fiber_resume(f, 1, &one);
}

/* Thread */

static VALUE
thread_fn(void *arg)
{
    return rb_sprintf("thread got %s", (const char *)arg);
}

static VALUE
thread_raise_fn(void *arg)
{
    (void)arg;
    rb_raise(rb_eRuntimeError, "raised in thread");
    return Qnil;
}

static VALUE
threads(VALUE self)
{
    static const char msg[] = "hello";
    VALUE out = rb_ary_new();
    rb_ary_push(out, rb_thread_alone() ? Qtrue : Qfalse);
    VALUE t = rb_thread_create(thread_fn, (void *)msg);
    rb_ary_push(out, rb_funcall(t, rb_intern("value"), 0));
    rb_ary_push(out, rb_thread_wakeup_alive(t));
    rb_ary_push(out, rb_equal(rb_thread_main(), rb_funcall(rb_cThread, rb_intern("main"), 0)));
    rb_ary_push(out, ruby_thread_has_gvl_p() ? Qtrue : Qfalse);
    rb_ary_push(out, rb_thread_interrupted(rb_thread_current()) ? Qtrue : Qfalse);
    struct timeval tv = {0, 1000};
    rb_thread_wait_for(tv);
    return out;
}

static VALUE
thread_dead_wakeup(VALUE self)
{
    VALUE t = rb_thread_create(thread_fn, (void *)"x");
    rb_funcall(t, rb_intern("join"), 0);
    return rb_thread_wakeup(t);
}

static VALUE
thread_raise(VALUE self)
{
    VALUE t = rb_thread_create(thread_raise_fn, NULL);
    rb_funcall(t, rb_intern("report_on_exception="), 1, Qfalse);
    return rb_funcall(t, rb_intern("join"), 0);
}

static void *
nogvl_probe(void *p)
{
    *(int *)p = ruby_thread_has_gvl_p();
    return NULL;
}

static VALUE
gvl_in_nogvl(VALUE self)
{
    int has = -1;
    rb_thread_call_without_gvl(nogvl_probe, &has, RUBY_UBF_IO, NULL);
    return INT2FIX(has);
}

/* Exceptions */

static VALUE
make_exception(VALUE self, VALUE args)
{
    return rb_make_exception((int)RARRAY_LEN(args), RARRAY_CONST_PTR(args));
}

static VALUE
syserr(VALUE self, VALUE n, VALUE arg)
{
    VALUE a = rb_syserr_new_str(NUM2INT(n), arg);
    return rb_ary_new_from_args(2, rb_obj_class(a), rb_funcall(a, rb_intern("message"), 0));
}

static VALUE
error_frozen(VALUE self)
{
    rb_error_frozen("thing");
    return Qnil;
}

static VALUE
error_frozen_object(VALUE self, VALUE obj)
{
    rb_error_frozen_object(obj);
    return Qnil;
}

static VALUE
frozen_error_raise(VALUE self, VALUE obj)
{
    rb_frozen_error_raise(obj, "custom %d", 42);
    return Qnil;
}

static VALUE
set_errinfo(VALUE self, VALUE v)
{
    rb_set_errinfo(v);
    VALUE r = rb_errinfo();
    rb_set_errinfo(Qnil);
    return r;
}

static VALUE
errnos(VALUE self)
{
    rb_errno_set(EAGAIN);
    int a = rb_errno();
    *rb_errno_ptr() = EINTR;
    return rb_ary_new_from_args(2, INT2FIX(a == EAGAIN), INT2FIX(errno == EINTR));
}

static VALUE
interrupt(VALUE self)
{
    rb_interrupt();
    return Qnil;
}

/* IO */

static VALUE
io_misc(VALUE self, VALUE r, VALUE w)
{
    VALUE out = rb_ary_new();
    rb_ary_push(out, rb_io_closed_p(r));
    ssize_t n = rb_io_bufwrite(w, "xyz", 3);
    rb_io_flush(w);
    rb_ary_push(out, LONG2NUM((long)n));
    rb_ary_push(out, rb_funcall(r, rb_intern("readpartial"), 1, INT2FIX(1)));
    rb_io_ungetbyte(r, INT2FIX('A'));
    rb_io_ungetc(r, rb_str_new_cstr("B"));
    rb_ary_push(out, rb_funcall(r, rb_intern("read"), 1, INT2FIX(4)));
    rb_ary_push(out, rb_io_timeout(r));
    rb_io_set_timeout(r, DBL2NUM(2.5));
    rb_ary_push(out, rb_io_timeout(r));
    return out;
}

static VALUE
io_path(VALUE self, VALUE io)
{
    return rb_io_path(io);
}

static VALUE
io_mode_append(VALUE self, VALUE io)
{
    return (rb_io_mode(io) & FMODE_APPEND) ? Qtrue : Qfalse;
}

static VALUE
io_maybe_wait_closed(VALUE self, VALUE io)
{
    return rb_io_maybe_wait(EAGAIN, io, INT2FIX(RUBY_IO_READABLE), Qnil);
}

/* `GetOpenFile` on an IO that was allocated and never initialized. */
static VALUE
io_get_open_file(VALUE self, VALUE io)
{
    rb_io_t *fptr;
    GetOpenFile(io, fptr);
    return INT2FIX(fptr->fd >= 0);
}

static VALUE
io_open_descriptor(VALUE self, VALUE path)
{
    int fd = rb_cloexec_open(StringValueCStr(path), O_RDONLY, 0);
    VALUE io = rb_io_open_descriptor(rb_cFile, fd, FMODE_READABLE, path, Qnil, NULL);
    VALUE r = rb_ary_new_from_args(3, rb_obj_class(io), rb_io_path(io), rb_funcall(io, rb_intern("gets"), 0));
    rb_io_close(io);
    return r;
}

static int
cloexec_p(int fd)
{
    return (fcntl(fd, F_GETFD) & FD_CLOEXEC) != 0;
}

static VALUE
cloexec(VALUE self)
{
    int p[2];
    VALUE out = rb_ary_new();
    rb_ary_push(out, INT2FIX(rb_cloexec_pipe(p)));
    rb_ary_push(out, cloexec_p(p[0]) && cloexec_p(p[1]) ? Qtrue : Qfalse);
    int d = rb_cloexec_dup(p[0]);
    rb_ary_push(out, d > 2 && cloexec_p(d) ? Qtrue : Qfalse);
    int e = rb_cloexec_fcntl_dupfd(p[1], 50);
    rb_ary_push(out, e >= 50 && cloexec_p(e) ? Qtrue : Qfalse);
    int f = rb_cloexec_dup2(p[0], 60);
    rb_ary_push(out, f == 60 && cloexec_p(f) ? Qtrue : Qfalse);
    close(p[0]);
    close(p[1]);
    close(d);
    close(e);
    close(f);
    int q[2];
    rb_ary_push(out, INT2FIX(rb_pipe(q)));
    FILE *fp = rb_fdopen(q[1], "w");
    fputs("via FILE\n", fp);
    fclose(fp);
    char buf[32] = {0};
    ssize_t n = read(q[0], buf, sizeof(buf) - 1);
    close(q[0]);
    rb_ary_push(out, rb_str_new(buf, n < 0 ? 0 : n));
    return out;
}

/* GC */

static VALUE
gc_misc(VALUE self)
{
    VALUE out = rb_ary_new();
    size_t c = rb_gc_count();
    rb_ary_push(out, c == rb_gc_count() ? Qtrue : Qfalse);
    rb_ary_push(out, rb_gc_stat(ID2SYM(rb_intern("count"))) >= c ? Qtrue : Qfalse);
    VALUE h = rb_hash_new();
    rb_gc_stat(h);
    rb_ary_push(out, RHASH_SIZE(h) > 0 ? Qtrue : Qfalse);
    rb_gc_adjust_memory_usage(1024);
    rb_gc_adjust_memory_usage(-1024);
    rb_gc_force_recycle(rb_str_new_cstr("b"));
    rb_gc_disable_no_rest();
    rb_gc_enable();
    return out;
}

static VALUE
latest_gc_info(VALUE self, VALUE key)
{
    return rb_gc_latest_gc_info(key);
}

static VALUE
finalizers(VALUE self, VALUE keep, VALUE drop, VALUE blk)
{
    rb_define_finalizer(keep, blk);
    rb_define_finalizer(drop, blk);
    rb_undefine_finalizer(drop);
    return Qnil;
}

/* Processes and files */

static VALUE
processes(VALUE self)
{
    VALUE out = rb_ary_new();
    VALUE cmd[2] = {rb_str_new_cstr("sh"), rb_str_new_cstr("-c")};
    VALUE argv[3] = {cmd[0], cmd[1], rb_str_new_cstr("exit 3")};
    rb_pid_t pid = rb_spawn(3, argv);
    int status = -1;
    rb_pid_t got = rb_waitpid(pid, &status, 0);
    rb_ary_push(out, got == pid ? Qtrue : Qfalse);
    rb_ary_push(out, INT2FIX(WEXITSTATUS(status)));
    rb_ary_push(out, rb_funcall(rb_last_status_get(), rb_intern("exitstatus"), 0));
    errno = 0;
    rb_pid_t none = rb_waitpid(pid, &status, 0);
    rb_ary_push(out, rb_ary_new_from_args(2, INT2FIX(none), INT2FIX(errno == ECHILD)));
    rb_ary_push(out, rb_obj_class(rb_proc_times(Qnil)));
    return out;
}

static VALUE
files(VALUE self)
{
    VALUE out = rb_ary_new();
    rb_ary_push(out, rb_file_expand_path(rb_str_new_cstr("../b"), rb_str_new_cstr("/x/y")));
    rb_ary_push(out, rb_file_absolute_path(rb_str_new_cstr("~/b"), rb_str_new_cstr("/x")));
    VALUE a[2] = {rb_str_new_cstr("c"), rb_str_new_cstr("/d")};
    rb_ary_push(out, rb_file_s_expand_path(2, a));
    rb_ary_push(out, rb_file_s_absolute_path(2, a));
    rb_ary_push(out, rb_file_dirname(rb_str_new_cstr("/a/b/c.rb")));
    rb_ary_push(out, rb_file_directory_p(Qnil, rb_str_new_cstr("/")));
    return out;
}

/* Odds and ends */

static VALUE
misc(VALUE self)
{
    VALUE out = rb_ary_new();
    rb_ary_push(out, rb_gcd(INT2FIX(12), INT2FIX(18)));
    rb_ary_push(out, rb_gcd_normal(INT2FIX(7), INT2FIX(21)));
    rb_ary_push(out, rb_str_new_cstr(ruby_signal_name(SIGINT)));
    rb_ary_push(out, rb_str_new_cstr(ruby_signal_name(SIGTERM)));
    ruby_setenv("CAPI_PROCIO_ENV", "set");
    rb_ary_push(out, rb_str_new_cstr(getenv("CAPI_PROCIO_ENV")));
    ruby_unsetenv("CAPI_PROCIO_ENV");
    rb_ary_push(out, getenv("CAPI_PROCIO_ENV") ? Qtrue : Qfalse);
    VALUE args[3] = {INT2FIX(10), DBL2NUM(2.5), rb_str_new_cstr("test")};
    rb_ary_push(out, rb_str_format(3, args, rb_str_new_cstr("%d %f %s")));
    return out;
}

static VALUE
source(VALUE self)
{
    const char *f = rb_sourcefile();
    const char *base = strrchr(f, '/');
    return rb_ary_new_from_args(2, rb_str_new_cstr(base ? base + 1 : f), INT2FIX(rb_sourceline()));
}

static VALUE
frame_info(VALUE self)
{
    ID id = 0;
    VALUE klass = Qnil;
    int r = rb_frame_method_id_and_class(&id, &klass);
    return rb_ary_new_from_args(3, INT2FIX(r), ID2SYM(id), klass);
}

static VALUE
size_fn(VALUE obj, VALUE args, VALUE eobj)
{
    return LONG2NUM(RARRAY_LEN(args) * 10 + (rb_obj_is_kind_of(eobj, rb_cEnumerator) ? 1 : 0));
}

static VALUE
enum_with_size(VALUE self, VALUE obj, VALUE args)
{
    return rb_enumeratorize_with_size(obj, ID2SYM(rb_intern("each")), (int)RARRAY_LEN(args),
                                      RARRAY_CONST_PTR(args), size_fn);
}

/* Classes */

static VALUE
class_new(VALUE self, VALUE super)
{
    return rb_class_new(super);
}

static VALUE
define_class(VALUE self, VALUE name, VALUE super)
{
    return rb_define_class(StringValueCStr(name), NIL_P(super) ? 0 : super);
}

static VALUE
define_class_under(VALUE self, VALUE outer, VALUE name, VALUE super)
{
    return rb_define_class_under(outer, StringValueCStr(name), NIL_P(super) ? 0 : super);
}

static VALUE
undef_method(VALUE self, VALUE klass, VALUE name)
{
    rb_undef_method(klass, StringValueCStr(name));
    return Qnil;
}

static VALUE
define_const(VALUE self, VALUE klass, VALUE name, VALUE val)
{
    rb_define_const(klass, StringValueCStr(name), val);
    return Qnil;
}

void
Init_capi_procio(void)
{
    m = rb_define_module("CapiProcio");
    rb_define_module_function(m, "proc_call_kw", proc_call_kw, 3);
    rb_define_module_function(m, "proc_call_with_block_kw", proc_call_with_block_kw, 3);
    rb_define_module_function(m, "method_calls", method_calls, 3);
    rb_define_module_function(m, "yield_kw", yield_kw, 1);
    rb_define_module_function(m, "yield_block_each", yield_block_each, 1);
    rb_define_module_function(m, "public_kw", public_kw, 3);
    rb_define_module_function(m, "eval_cmd", eval_cmd, 2);
    rb_define_module_function(m, "basic_p", basic_p, 2);
    rb_define_module_function(m, "fibers", fibers, 0);
    rb_define_module_function(m, "fiber_raise", fiber_raise, 0);
    rb_define_module_function(m, "fiber_storage", fiber_storage, 1);
    rb_define_module_function(m, "threads", threads, 0);
    rb_define_module_function(m, "thread_dead_wakeup", thread_dead_wakeup, 0);
    rb_define_module_function(m, "thread_raise", thread_raise, 0);
    rb_define_module_function(m, "gvl_in_nogvl", gvl_in_nogvl, 0);
    rb_define_module_function(m, "make_exception", make_exception, 1);
    rb_define_module_function(m, "syserr", syserr, 2);
    rb_define_module_function(m, "error_frozen", error_frozen, 0);
    rb_define_module_function(m, "error_frozen_object", error_frozen_object, 1);
    rb_define_module_function(m, "frozen_error_raise", frozen_error_raise, 1);
    rb_define_module_function(m, "set_errinfo", set_errinfo, 1);
    rb_define_module_function(m, "errnos", errnos, 0);
    rb_define_module_function(m, "interrupt", interrupt, 0);
    rb_define_module_function(m, "io_misc", io_misc, 2);
    rb_define_module_function(m, "io_path", io_path, 1);
    rb_define_module_function(m, "io_mode_append", io_mode_append, 1);
    rb_define_module_function(m, "io_maybe_wait_closed", io_maybe_wait_closed, 1);
    rb_define_module_function(m, "io_get_open_file", io_get_open_file, 1);
    rb_define_module_function(m, "io_open_descriptor", io_open_descriptor, 1);
    rb_define_module_function(m, "cloexec", cloexec, 0);
    rb_define_module_function(m, "gc_misc", gc_misc, 0);
    rb_define_module_function(m, "latest_gc_info", latest_gc_info, 1);
    rb_define_module_function(m, "finalizers", finalizers, 3);
    rb_define_module_function(m, "processes", processes, 0);
    rb_define_module_function(m, "files", files, 0);
    rb_define_module_function(m, "misc", misc, 0);
    rb_define_module_function(m, "source", source, 0);
    rb_define_module_function(m, "frame_info", frame_info, 0);
    rb_define_module_function(m, "enum_with_size", enum_with_size, 2);
    rb_define_module_function(m, "class_new", class_new, 1);
    rb_define_module_function(m, "define_class", define_class, 2);
    rb_define_module_function(m, "define_class_under", define_class_under, 3);
    rb_define_module_function(m, "undef_method", undef_method, 2);
    rb_define_module_function(m, "define_const", define_const, 3);
}
