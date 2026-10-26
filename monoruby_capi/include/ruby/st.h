/*
 * ruby/st.h — CRuby's st_table hash table API.
 *
 * The declarations and the `st_table` layout are CRuby's (include/ruby/st.h);
 * the implementation is monoruby_capi/src/st.c, CRuby's own st.c carried
 * over. As in CRuby every function is really `rb_st_*`, reached through the
 * `st_*` macros below, so a table is laid out and probed exactly as the
 * extension's author tested it against CRuby. Entries are kept in
 * insertion order; `st_data_t` is wide enough for a VALUE or a pointer.
 *
 * Not provided: `rb_hash_bulk_insert_into_st_table` (CRuby's Hash is not
 * an st_table here).
 */
#ifndef MONORUBY_CAPI_RUBY_ST_H
#define MONORUBY_CAPI_RUBY_ST_H 1

#include <stddef.h>
#include <stdint.h>
#include <limits.h>
#include "ruby/config.h"

#ifdef __cplusplus
extern "C" {
#endif

RUBY_SYMBOL_EXPORT_BEGIN

#ifndef ST_DATA_T_DEFINED
typedef uintptr_t st_data_t;
#define ST_DATA_T_DEFINED
#endif
#ifndef MONORUBY_CAPI_ST_INDEX_T_DEFINED
typedef st_data_t st_index_t;
#define MONORUBY_CAPI_ST_INDEX_T_DEFINED
#endif
#define MAX_ST_INDEX_VAL (~(st_index_t) 0)
#define SIZEOF_ST_INDEX_T SIZEOF_VOIDP
#define ST_INDEX_BITS (SIZEOF_ST_INDEX_T * CHAR_BIT)

typedef struct st_table st_table;
typedef int st_compare_func(st_data_t, st_data_t);
typedef st_index_t st_hash_func(st_data_t);

struct st_hash_type {
    int (*compare)(st_data_t, st_data_t); /* st_compare_func* */
    st_index_t (*hash)(st_data_t);        /* st_hash_func* */
};

typedef struct st_table_entry st_table_entry;
struct st_table_entry; /* defined in st.c */

struct st_table {
    /* Cached features of the table -- see st.c for more details.  */
    unsigned char entry_power, bin_power, size_ind;
    /* How many times the table was rebuilt.  */
    unsigned int rebuilds_num;
    const struct st_hash_type *type;
    /* Number of entries currently in the table.  */
    st_index_t num_entries;
    /* Array of bins used for access by keys.  */
    st_index_t *bins;
    /* Start and bound index of entries in array entries.
       entries_starts and entries_bound are in interval
       [0,allocated_entries].  */
    st_index_t entries_start, entries_bound;
    /* Array of size 2^entry_power.  */
    st_table_entry *entries;
};

#define st_is_member(table,key) st_lookup((table),(key),(st_data_t *)0)

enum st_retval {ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK, ST_REPLACE};

size_t rb_st_table_size(const struct st_table *tbl);
#define st_table_size rb_st_table_size
st_table *rb_st_init_table(const struct st_hash_type *);
#define st_init_table rb_st_init_table
st_table *rb_st_init_table_with_size(const struct st_hash_type *, st_index_t);
#define st_init_table_with_size rb_st_init_table_with_size
st_table *rb_st_init_existing_table_with_size(st_table *, const struct st_hash_type *, st_index_t);
#define st_init_existing_table_with_size rb_st_init_existing_table_with_size
st_table *rb_st_init_numtable(void);
#define st_init_numtable rb_st_init_numtable
st_table *rb_st_init_numtable_with_size(st_index_t);
#define st_init_numtable_with_size rb_st_init_numtable_with_size
st_table *rb_st_init_strtable(void);
#define st_init_strtable rb_st_init_strtable
st_table *rb_st_init_strtable_with_size(st_index_t);
#define st_init_strtable_with_size rb_st_init_strtable_with_size
st_table *rb_st_init_strcasetable(void);
#define st_init_strcasetable rb_st_init_strcasetable
st_table *rb_st_init_strcasetable_with_size(st_index_t);
#define st_init_strcasetable_with_size rb_st_init_strcasetable_with_size
int rb_st_delete(st_table *, st_data_t *, st_data_t *); /* returns 0:notfound 1:deleted */
#define st_delete rb_st_delete
int rb_st_delete_safe(st_table *, st_data_t *, st_data_t *, st_data_t);
#define st_delete_safe rb_st_delete_safe
int rb_st_shift(st_table *, st_data_t *, st_data_t *); /* returns 0:notfound 1:deleted */
#define st_shift rb_st_shift
int rb_st_insert(st_table *, st_data_t, st_data_t);
#define st_insert rb_st_insert
int rb_st_insert2(st_table *, st_data_t, st_data_t, st_data_t (*)(st_data_t));
#define st_insert2 rb_st_insert2
int rb_st_lookup(st_table *, st_data_t, st_data_t *);
#define st_lookup rb_st_lookup
int rb_st_get_key(st_table *, st_data_t, st_data_t *);
#define st_get_key rb_st_get_key
typedef int st_update_callback_func(st_data_t *key, st_data_t *value, st_data_t arg, int existing);
/* *key may be altered, but must equal to the old key, i.e., the
 * results of hash() are same and compare() returns 0, otherwise the
 * behavior is undefined */
int rb_st_update(st_table *table, st_data_t key, st_update_callback_func *func, st_data_t arg);
#define st_update rb_st_update
typedef int st_foreach_callback_func(st_data_t, st_data_t, st_data_t);
typedef int st_foreach_check_callback_func(st_data_t, st_data_t, st_data_t, int);
int rb_st_foreach_with_replace(st_table *tab, st_foreach_check_callback_func *func, st_update_callback_func *replace, st_data_t arg);
#define st_foreach_with_replace rb_st_foreach_with_replace
int rb_st_foreach(st_table *, st_foreach_callback_func *, st_data_t);
#define st_foreach rb_st_foreach
int rb_st_foreach_check(st_table *, st_foreach_check_callback_func *, st_data_t, st_data_t);
#define st_foreach_check rb_st_foreach_check
st_index_t rb_st_keys(st_table *table, st_data_t *keys, st_index_t size);
#define st_keys rb_st_keys
st_index_t rb_st_keys_check(st_table *table, st_data_t *keys, st_index_t size, st_data_t never);
#define st_keys_check rb_st_keys_check
st_index_t rb_st_values(st_table *table, st_data_t *values, st_index_t size);
#define st_values rb_st_values
st_index_t rb_st_values_check(st_table *table, st_data_t *values, st_index_t size, st_data_t never);
#define st_values_check rb_st_values_check
void rb_st_add_direct(st_table *, st_data_t, st_data_t);
#define st_add_direct rb_st_add_direct
void rb_st_add_direct_with_hash(st_table *, st_data_t, st_data_t, st_index_t);
void rb_st_free_table(st_table *);
#define st_free_table rb_st_free_table
void rb_st_cleanup_safe(st_table *, st_data_t);
#define st_cleanup_safe rb_st_cleanup_safe
void rb_st_clear(st_table *);
#define st_clear rb_st_clear
st_table *rb_st_copy(st_table *);
#define st_copy rb_st_copy
st_table *rb_st_replace(st_table *, st_table *);
#define st_replace rb_st_replace
int rb_st_numcmp(st_data_t, st_data_t);
#define st_numcmp rb_st_numcmp
st_index_t rb_st_numhash(st_data_t);
#define st_numhash rb_st_numhash
int rb_st_locale_insensitive_strcasecmp(const char *s1, const char *s2);
#define st_locale_insensitive_strcasecmp rb_st_locale_insensitive_strcasecmp
int rb_st_locale_insensitive_strncasecmp(const char *s1, const char *s2, size_t n);
#define st_locale_insensitive_strncasecmp rb_st_locale_insensitive_strncasecmp
#define st_strcasecmp rb_st_locale_insensitive_strcasecmp
#define st_strncasecmp rb_st_locale_insensitive_strncasecmp
size_t rb_st_memsize(const st_table *);
#define st_memsize rb_st_memsize
st_index_t rb_st_hash(const void *ptr, size_t len, st_index_t h);
#define st_hash rb_st_hash
st_index_t rb_st_hash_uint32(st_index_t h, uint32_t i);
#define st_hash_uint32 rb_st_hash_uint32
st_index_t rb_st_hash_uint(st_index_t h, st_index_t i);
#define st_hash_uint rb_st_hash_uint
st_index_t rb_st_hash_end(st_index_t h);
#define st_hash_end rb_st_hash_end
st_index_t rb_st_hash_start(st_index_t h);
#define st_hash_start(h) ((st_index_t)(h))
/* The rb_hash_* spellings of the incremental hash API. */
#define rb_hash_start(h) st_hash_start(h)
#define rb_hash_uint32(h, i) st_hash_uint32((h), (i))
#define rb_hash_uint(h, i) st_hash_uint((h), (i))
#define rb_hash_end(h) st_hash_end(h)

RUBY_SYMBOL_EXPORT_END

#ifdef __cplusplus
}
#endif

#endif /* MONORUBY_CAPI_RUBY_ST_H */
