/*
 * monoruby_ext.h — the C ABI between monoruby and a dynamically loaded
 * extension. The C rendering of ../src/lib.rs; keep the two in step.
 *
 * An extension is a shared library exporting
 *
 *     int Init_<name>(MrContext *ctx);
 *
 * loaded by `require "<name>.so"`. Everything it needs from the
 * interpreter is reached through `ctx->api` — it never links against
 * monoruby. This is not the CRuby C API: an MrValue has monoruby's bit
 * layout, errors are returned (raise + return MR_UNDEF), never unwound,
 * and the interpreter is reached only through the table.
 *
 * Rules: (1) call only through ctx->api; (2) never unwind across the
 * boundary; (3) every MrValue held across a call back into Ruby
 * (funcall / yield_block / call_blocking) is on the temp stack, reachable
 * from a native object's mark, or pinned; (4) kernel-blocking calls go
 * through call_blocking; (5) Init_ runs once, at load.
 *
 * See doc/native_extension_loading.md §4.
 */
#ifndef MONORUBY_EXT_H
#define MONORUBY_EXT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MR_ABI_VERSION 1u

typedef uint64_t MrValue;

/* An interned name, from `intern`. Interning is per process and
 * permanent, so one taken during Init_ stays valid for every later
 * call. MR_NO_SYM is the one invalid value. */
typedef uint32_t MrSym;
#define MR_NO_SYM ((MrSym)0)

#define MR_UNDEF ((MrValue)0)    /* not a value: error pending / absent */
#define MR_NIL   ((MrValue)0x04)
#define MR_FALSE ((MrValue)0x14)
#define MR_TRUE  ((MrValue)0x1c)

typedef struct MrApi MrApi;

/* Only `api` is for the extension; the rest is monoruby's, valid for the
 * duration of the call the context was handed to. */
typedef struct MrContext {
    const MrApi *api;
    void *vm;
    void *globals;
    void *frame;
    void *pc;
} MrContext;

/* A method: argv[0..argc] are the positional arguments, block the block
 * handle (MR_UNDEF when none). Returns the result, or MR_UNDEF with an
 * error pending. */
typedef MrValue (*MrMethodFn)(MrContext *ctx, MrValue self, int argc,
                              const MrValue *argv, MrValue block);
/* A method registered with define_method_data: MrMethodFn plus the data
 * pointer given at registration. */
typedef MrValue (*MrMethodDataFn)(MrContext *ctx, MrValue self, int argc,
                                  const MrValue *argv, MrValue block, void *data);
/* Init_<name>: 0 on success. */
typedef int (*MrInitFn)(MrContext *ctx);
/* Run by call_blocking on a worker thread; must not touch Ruby. */
typedef int64_t (*MrBlockingFn)(void *arg);
/* The hooks of a global variable registered with gvar_hook: the getter
 * returns the value (MR_UNDEF with an error pending), the setter 0 (-1
 * with an error pending). */
typedef MrValue (*MrGvarGetFn)(MrContext *ctx, void *data);
typedef int (*MrGvarSetFn)(MrContext *ctx, void *data, MrValue val);

typedef struct MrMarker MrMarker; /* opaque; only gc_mark takes it */

/* The callbacks of one kind of native object. The address of the struct
 * is the kind's identity (native_data checks it), so keep one static
 * instance per kind. */
typedef struct MrNativeOps {
    const char *name;
    void (*mark)(void *data, MrMarker *marker); /* gc_mark each held value; NULL if none */
    void (*free)(void *data);                   /* NULL if nothing to free */
    void *(*dup)(void *data);                   /* NULL: cannot be copied */
} MrNativeOps;

typedef enum MrType {
    MR_T_UNDEF = 0, MR_T_NIL, MR_T_TRUE, MR_T_FALSE, MR_T_INTEGER, MR_T_FLOAT,
    MR_T_SYMBOL, MR_T_STRING, MR_T_ARRAY, MR_T_HASH, MR_T_CLASS, MR_T_MODULE,
    MR_T_PROC, MR_T_EXCEPTION, MR_T_NATIVE, MR_T_OBJECT
} MrType;

typedef enum MrErrorKind {
    MR_E_RUNTIME = 0, MR_E_ARGUMENT, MR_E_TYPE, MR_E_RANGE, MR_E_INDEX,
    MR_E_IO, MR_E_NOT_IMPLEMENTED, MR_E_FROZEN, MR_E_LOAD
} MrErrorKind;

#define MR_METHOD_PRIVATE   1u
#define MR_METHOD_SINGLETON 2u
#define MR_CLASS_NATIVE     1u
#define MR_ARGC_VARIADIC    (-1)
#define MR_ALLOC_NATIVE     1u   /* class_set_alloc: instances carry a native payload */
#define MR_ALLOC_UNDEF      2u   /* class_set_alloc: no allocator */

/* Entries are only ever appended; `size` says how many bytes the running
 * monoruby fills. */
/* MrApi::io_state's result; `mode` holds CRuby's FMODE_* bits, `rbuf` the
 * bytes read from the descriptor and not yet consumed. */
typedef struct MrIoState {
    int fd;
    int mode;
    const uint8_t *rbuf;
    size_t rbuf_len;
} MrIoState;

struct MrApi {
    uint32_t abi_version;
    uint32_t size;

    /* errors */
    void    (*raise)(MrContext *, MrValue exc_class, const char *msg, size_t len);
    void    (*raise_kind)(MrContext *, MrErrorKind kind, const char *msg, size_t len);
    int     (*error_pending)(MrContext *);
    MrValue (*error_take)(MrContext *);
    void    (*raise_exception)(MrContext *, MrValue exc);

    /* classes */
    MrValue (*object_class)(MrContext *);
    MrValue (*define_class)(MrContext *, MrValue outer, const char *name, MrValue superclass, uint32_t flags);
    MrValue (*define_module)(MrContext *, MrValue outer, const char *name);
    void    (*define_method)(MrContext *, MrValue klass, const char *name, MrMethodFn f, int argc, uint32_t flags);
    MrValue (*const_get)(MrContext *, MrValue outer, const char *name);
    void    (*const_set)(MrContext *, MrValue outer, const char *name, MrValue val);

    /* values */
    MrType  (*type_of)(MrContext *, MrValue v);
    MrValue (*class_of)(MrContext *, MrValue v);
    int     (*is_kind_of)(MrContext *, MrValue v, MrValue klass);
    MrValue (*int_new)(int64_t i);
    int     (*int_get)(MrContext *, MrValue v, int64_t *out);
    MrValue (*float_new)(double f);
    int     (*float_get)(MrContext *, MrValue v, double *out);
    MrValue (*str_new)(MrContext *, const uint8_t *ptr, size_t len);
    MrValue (*bytes_new)(MrContext *, const uint8_t *ptr, size_t len);
    const uint8_t *(*str_ptr)(MrContext *, MrValue v, size_t *len);
    MrValue (*str_encoding)(MrContext *, MrValue v);   /* the encoding's name as a String */
    MrValue (*sym_new)(MrContext *, const uint8_t *ptr, size_t len);
    MrValue (*sym_to_str)(MrContext *, MrValue v);
    MrValue (*ary_new)(MrContext *);
    int     (*ary_push)(MrContext *, MrValue ary, MrValue v);
    size_t  (*ary_len)(MrContext *, MrValue ary);
    MrValue (*ary_get)(MrContext *, MrValue ary, size_t idx);
    MrValue (*hash_new)(MrContext *);
    int     (*hash_set)(MrContext *, MrValue hash, MrValue key, MrValue v);
    MrValue (*hash_get)(MrContext *, MrValue hash, MrValue key);
    MrValue (*ivar_get)(MrContext *, MrValue obj, const char *name);
    int     (*ivar_set)(MrContext *, MrValue obj, const char *name, MrValue v);
    MrValue (*inspect)(MrContext *, MrValue v);
    int     (*respond_to)(MrContext *, MrValue v, const char *name);  /* method lookup only */

    /* native objects */
    MrValue (*native_new)(MrContext *, MrValue klass, void *data, const MrNativeOps *ops);
    void   *(*native_data)(MrContext *, MrValue obj, const MrNativeOps *ops);
    int     (*native_set)(MrContext *, MrValue obj, void *data, const MrNativeOps *ops);

    /* GC */
    void    (*gc_mark)(MrMarker *marker, MrValue v);
    void    (*temp_push)(MrContext *, MrValue v);
    size_t  (*temp_len)(MrContext *);
    void    (*temp_truncate)(MrContext *, size_t len);
    void    (*gc_pin)(MrContext *, MrValue v);
    void    (*gc_unpin)(MrContext *, MrValue v);

    /* calling Ruby */
    MrValue (*funcall)(MrContext *, MrValue recv, const char *name, int argc, const MrValue *argv, MrValue block);
    MrValue (*yield_block)(MrContext *, MrValue block, int argc, const MrValue *argv);
    MrValue (*block_to_proc)(MrContext *, MrValue block);
    MrValue (*proc_call)(MrContext *, MrValue proc, int argc, const MrValue *argv);

    /* threads */
    int     (*call_blocking)(MrContext *, MrBlockingFn f, void *arg, int64_t *out);

    /* misc */
    const char *(*ruby_version)(void);

    /* appended after the first release */
    MrSym   (*intern)(MrContext *, const uint8_t *ptr, size_t len);
    MrValue (*funcall_sym)(MrContext *, MrValue recv, MrSym sym, int argc, const MrValue *argv, MrValue block);

    /* appended for the CRuby C API layer (monoruby_capi/) */
    void    (*define_method_data)(MrContext *, MrValue klass, const char *name, MrMethodDataFn f, void *data, int argc, uint32_t flags);
    uint8_t *(*str_cptr)(MrContext *, MrValue v, size_t *len);   /* own buffer, NUL-terminated, writable */
    int     (*str_resize)(MrContext *, MrValue v, size_t len);   /* truncate / NUL-pad */
    int     (*str_cat)(MrContext *, MrValue v, const uint8_t *ptr, size_t len);
    const MrValue *(*ary_ptr)(MrContext *, MrValue ary, size_t *len);   /* read-only */
    void    (*gc_register_address)(MrContext *, const MrValue *addr);
    void    (*gc_unregister_address)(MrContext *, const MrValue *addr);
    int     (*class_set_alloc)(MrContext *, MrValue klass, uint32_t flags);   /* MR_ALLOC_* */
    void   *(*native_take)(MrContext *, MrValue obj);   /* detach the payload, unfreed */
    MrValue (*call_super)(MrContext *, int argc, const MrValue *argv, MrValue block);

    /* appended for ruby/re.h: Onigmo through the table (see MrOnigApi) */
    void   *(*regexp_prepare)(MrContext *, MrValue re, MrValue str);   /* regex_t * for a match on str; monoruby's, never onig_free */
    void   *(*regexp_ptr)(MrContext *, MrValue re);                    /* regex_t * under re's own codec */
    const struct MrOnigApi *(*onig_api)(void);
    int     (*enc_precise_mbclen)(MrContext *, const char *name, const unsigned char *p, const unsigned char *e); /* Onigmo's MBCLEN encoding */
    int     (*keyword_given)(MrContext *);                              /* the entering call passed keywords (rb_keyword_given_p) */
    int64_t (*enc_codepoint)(MrContext *, const char *name, const unsigned char *p, const unsigned char *e, int *len); /* code point at p, or -1 */

    /* appended for rb_backref_get / rb_backref_set: `$~` of the Ruby frame the running extension method was called from */
    MrValue (*backref_get)(MrContext *);
    void    (*backref_set)(MrContext *, MrValue md);

    /* appended for rb_thread_call_without_gvl's unblock function: call_blocking whose
       interrupt (Thread#wakeup / raise / kill, a signal on main) runs ubf(ubf_arg) once.
       ubf NULL = none, (void *)-1 = RUBY_UBF_IO (the interrupt's signal gives EINTR) */
    int     (*call_blocking_ubf)(MrContext *, MrBlockingFn f, void *arg, void *ubf, void *ubf_arg, int64_t *out);

    /* appended for rb_define_hooked_variable and its relatives: reads of the global
       `name` call getter(ctx, data), assignments setter(ctx, data, val); setter NULL
       makes it read-only, getter NULL reads as nil. The hooks' own values are theirs
       to keep alive (gc_register_address / gc_pin). */
    void    (*gvar_hook)(MrContext *, MrSym name, void *data, MrGvarGetFn getter, MrGvarSetFn setter);
    /* appended for rb_lastline_get / rb_lastline_set (`$_` of the calling Ruby frame) and OBJ_FROZEN */
    MrValue (*lastline_get)(MrContext *);
    void    (*lastline_set)(MrContext *, MrValue v);
    int     (*obj_frozen)(MrContext *, MrValue v);                       /* v.frozen? without a method call */
    MrValue (*gvar_get)(MrContext *, const char *name);                  /* rb_gv_get: `$name` with its hook */
    int     (*gvar_set)(MrContext *, const char *name, MrValue v);       /* rb_gv_set: -1 with an error pending */
    MrValue (*funcall_kw)(MrContext *, MrValue recv, MrSym sym, int argc, const MrValue *argv, MrValue block, int kw_splat); /* last of argv as keywords */
    uint8_t *(*str_rptr)(MrContext *, MrValue v, size_t *len);   /* RSTRING_PTR: str_cptr, or a shared view's bytes in place */
    MrValue (*str_subseq)(MrContext *, MrValue v, int64_t beg, int64_t len); /* rb_str_subseq (clamped) */
    const char *(*str_enc_name)(MrContext *, MrValue v);             /* encoding name, one static pointer per encoding */
    int     (*str_set_enc)(MrContext *, MrValue v, const char *name);  /* force_encoding: 0, -1 error, -2 unresolved */
    int     (*str_coderange)(MrContext *, MrValue v);  /* 1 7bit, 2 valid, 3 broken, -1 error */
    MrValue (*str_dup)(MrContext *, MrValue v);  /* rb_str_dup */
    int64_t (*str_capacity)(MrContext *, MrValue v);  /* rb_str_capacity, -1 error */
    MrValue (*thread_current)(MrContext *);  /* rb_thread_current */
    MrValue (*thread_local_aref)(MrContext *, MrValue thread, MrSym sym);  /* thread[sym], no dispatch */
    void    (*obj_freeze)(MrContext *, MrValue v);  /* Kernel#freeze, no dispatch */
    int     (*io_state)(MrContext *, MrValue io, MrIoState *out);  /* struct rb_io view, -1 error */
    int     (*io_update)(MrContext *, MrValue io, int fd, int mode, size_t rbuf_consumed);  /* write it back, -1 error */
    int     (*str_modify)(MrContext *, MrValue v);  /* rb_str_modify, keeps bytes past len; -1 error */
};

/* Onigmo's entry points as linked into monoruby, types erased: regex_t *
 * and OnigRegion * are void *, OnigPosition is ptrdiff_t. */
typedef struct MrOnigApi {
    ptrdiff_t (*search)(void *reg, const uint8_t *str, const uint8_t *end, const uint8_t *start, const uint8_t *range, void *region, uint32_t option);
    ptrdiff_t (*match_at)(void *reg, const uint8_t *str, const uint8_t *end, const uint8_t *at, void *region, uint32_t option);
    void   *(*region_new)(void);
    void    (*region_init)(void *region);
    void    (*region_free)(void *region, int free_self);
    void    (*region_clear)(void *region);
    void    (*region_copy)(void *to, const void *from);
    int     (*region_resize)(void *region, int n);
    int     (*region_set)(void *region, int at, int beg, int end);
    void    (*free)(void *reg);
    int     (*name_to_backref_number)(void *reg, const uint8_t *name, const uint8_t *name_end, const void *region);
    int     (*foreach_name)(void *reg, int (*func)(const uint8_t *, const uint8_t *, int, int *, void *, void *), void *arg);
    int     (*number_of_names)(const void *reg);
    int     (*number_of_captures)(const void *reg);
    int     (*number_of_capture_histories)(const void *reg);
    int     (*error_code_to_str)(uint8_t *buf, ptrdiff_t code);
} MrOnigApi;

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_EXT_H */
