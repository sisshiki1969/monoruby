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
/* Init_<name>: 0 on success. */
typedef int (*MrInitFn)(MrContext *ctx);
/* Run by call_blocking on a worker thread; must not touch Ruby. */
typedef int64_t (*MrBlockingFn)(void *arg);

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

/* Entries are only ever appended; `size` says how many bytes the running
 * monoruby fills. */
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
};

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_EXT_H */
