/* The extension tests/native_ext.rs compiles and loads: one method of
 * every shape the ABI offers, over the C header — so a drift between
 * include/monoruby_ext.h and monoruby_ext_sys/src/lib.rs shows up here. */
#include "monoruby_ext.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static const MrApi *api;

/* Hello.add(a, b) */
static MrValue hello_add(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    int64_t a, b;
    if (!api->int_get(ctx, argv[0], &a) || !api->int_get(ctx, argv[1], &b)) return MR_UNDEF;
    return api->int_new(a + b);
}

/* Hello.greet(name) -> "hello, <name>" */
static MrValue hello_greet(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    size_t len; const uint8_t *p = api->str_ptr(ctx, argv[0], &len);
    if (!p) return MR_UNDEF;
    char buf[256]; int n = snprintf(buf, sizeof buf, "hello, %.*s", (int)len, (const char*)p);
    return api->str_new(ctx, (const uint8_t*)buf, n);
}

/* Hello.boom(msg) -> raises Hello::Error */
static MrValue hello_boom(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    MrValue hello = api->const_get(ctx, MR_UNDEF, "Hello");
    MrValue err = api->const_get(ctx, hello, "Error");
    size_t len; const uint8_t *p = api->str_ptr(ctx, argv[0], &len);
    if (!p) return MR_UNDEF;
    api->raise(ctx, err, (const char*)p, len);
    return MR_UNDEF;
}

/* Hello.each3 { |i| } -> yields 0,1,2, returns array of results */
static MrValue hello_each3(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    MrValue ary = api->ary_new(ctx);
    api->temp_push(ctx, ary);
    for (int i = 0; i < 3; i++) {
        MrValue a = api->int_new(i);
        MrValue r = api->yield_block(ctx, block, 1, &a);
        if (r == MR_UNDEF) return MR_UNDEF;
        api->ary_push(ctx, ary, r);
    }
    return ary;
}

/* Hello.variadic(*args) -> args.size and a Hash {sym: argc} */
static MrValue hello_variadic(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    MrValue h = api->hash_new(ctx);
    api->hash_set(ctx, h, api->sym_new(ctx, (const uint8_t*)"argc", 4), api->int_new(argc));
    MrValue f = api->float_new(1.5);
    api->hash_set(ctx, h, api->sym_new(ctx, (const uint8_t*)"f", 1), f);
    return h;
}

/* Hello.call_it(obj, name) -> obj.send(name) via funcall, propagating errors */
static MrValue hello_call_it(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    MrValue s = api->sym_to_str(ctx, argv[1]);
    if (s == MR_UNDEF) return MR_UNDEF;
    size_t len; const uint8_t *p = api->str_ptr(ctx, s, &len);
    char name[64]; snprintf(name, sizeof name, "%.*s", (int)len, (const char*)p);
    return api->funcall(ctx, argv[0], name, 0, NULL, MR_UNDEF);
}

/* ---- a native class: Hello::Counter with a held Ruby value ---- */
typedef struct { int64_t n; MrValue tag; } Counter;
static void counter_mark(void *d, MrMarker *m) { api->gc_mark(m, ((Counter*)d)->tag); }
static void counter_free(void *d) { free(d); }
static void *counter_dup(void *d) { Counter *c = malloc(sizeof *c); *c = *(Counter*)d; return c; }
static const MrNativeOps COUNTER_OPS = { "Hello::Counter", counter_mark, counter_free, counter_dup };

static MrValue counter_initialize(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    Counter *c = malloc(sizeof *c); c->n = 0; c->tag = argc > 0 ? argv[0] : MR_NIL;
    if (api->native_set(ctx, self, c, &COUNTER_OPS) != 0) return MR_UNDEF;
    return MR_NIL;
}
static MrValue counter_incr(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    Counter *c = api->native_data(ctx, self, &COUNTER_OPS);
    if (!c) { api->raise_kind(ctx, MR_E_TYPE, "not a counter", 13); return MR_UNDEF; }
    return api->int_new(++c->n);
}
static MrValue counter_tag(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    Counter *c = api->native_data(ctx, self, &COUNTER_OPS);
    if (!c) { api->raise_kind(ctx, MR_E_TYPE, "not a counter", 13); return MR_UNDEF; }
    return c->tag;
}

/* blocking: sleep-ish work on the pool */
static int64_t slow_sum(void *arg) { int64_t n = *(int64_t*)arg, s = 0; for (int64_t i = 0; i < n; i++) s += i; return s; }
static MrValue hello_blocking(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    int64_t n, out; if (!api->int_get(ctx, argv[0], &n)) return MR_UNDEF;
    if (api->call_blocking(ctx, slow_sum, &n, &out) != 0) return MR_UNDEF;
    return api->int_new(out);
}

/* stash an error across "C frames", then re-raise */
static MrValue hello_stash(MrContext *ctx, MrValue self, int argc, const MrValue *argv, MrValue block) {
    MrValue r = api->yield_block(ctx, block, 0, NULL);
    if (r != MR_UNDEF) return r;
    MrValue ex = api->error_take(ctx);
    if (api->error_pending(ctx)) return MR_UNDEF; /* should not happen */
    MrValue cls = api->class_of(ctx, ex);
    api->temp_push(ctx, ex);
    MrValue ins = api->inspect(ctx, cls);
    (void)ins;
    api->raise_exception(ctx, ex);
    return MR_UNDEF;
}

int Init_hello_ext(MrContext *ctx) {
    api = ctx->api;
    if (api->abi_version != MR_ABI_VERSION) return 1;
    MrValue hello = api->define_module(ctx, MR_UNDEF, "Hello");
    MrValue std_err = api->const_get(ctx, MR_UNDEF, "StandardError");
    api->define_class(ctx, hello, "Error", std_err, 0);
    api->define_method(ctx, hello, "add", hello_add, 2, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "greet", hello_greet, 1, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "boom", hello_boom, 1, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "each3", hello_each3, 0, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "variadic", hello_variadic, MR_ARGC_VARIADIC, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "call_it", hello_call_it, 2, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "blocking", hello_blocking, 1, MR_METHOD_SINGLETON);
    api->define_method(ctx, hello, "stash", hello_stash, 0, MR_METHOD_SINGLETON);
    MrValue counter = api->define_class(ctx, hello, "Counter", MR_UNDEF, MR_CLASS_NATIVE);
    api->define_method(ctx, counter, "initialize", counter_initialize, MR_ARGC_VARIADIC, MR_METHOD_PRIVATE);
    api->define_method(ctx, counter, "incr", counter_incr, 0, 0);
    api->define_method(ctx, counter, "tag", counter_tag, 0, 0);
    api->const_set(ctx, hello, "VERSION", api->str_new(ctx, (const uint8_t*)api->ruby_version(), strlen(api->ruby_version())));
    return 0;
}
