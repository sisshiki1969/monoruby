/*
 * Stage 6 of the CRuby-compatible C API: ruby/re.h — Onigmo run by the
 * extension itself, the way strscan does it. Written against CRuby's
 * headers only; built and run on both interpreters by tests/capi.rs,
 * which compares the output. Positions are byte offsets, as Onigmo's
 * are on both sides.
 */
#include <ruby.h>
#include <ruby/re.h>
#include <ruby/encoding.h>
#include <string.h>

/* CRuby exports onig_region_memsize (re.c) but declares it in no public
 * header; strscan declares it itself, so does this fixture. monoruby's
 * ruby/re.h declares it too, compatibly. */
extern size_t onig_region_memsize(const struct re_registers *regs);

static VALUE mRe;

struct search_args {
    long pos;
    int anchored;
};

/* The match function rb_reg_onig_match drives: onig_match at pos, or
 * onig_search from it. */
static OnigPosition
match_fn(regex_t *reg, VALUE str, struct re_registers *regs, void *arg)
{
    struct search_args *a = arg;
    const UChar *s = (const UChar *)RSTRING_PTR(str);
    const UChar *e = s + RSTRING_LEN(str);
    if (a->anchored)
        return onig_match(reg, s, e, s + a->pos, regs, ONIG_OPTION_NONE);
    return onig_search(reg, s, e, s + a->pos, e, regs, ONIG_OPTION_NONE);
}

static VALUE
regs_to_ary(struct re_registers *regs)
{
    VALUE ary = rb_ary_new();
    int i;
    for (i = 0; i < regs->num_regs; i++) {
        rb_ary_push(ary, rb_assoc_new(LONG2NUM((long)regs->beg[i]), LONG2NUM((long)regs->end[i])));
    }
    return ary;
}

/* Re.run(re, str, pos, anchored) -> [position, [[beg, end], ...]] | nil */
static VALUE
re_run(VALUE self, VALUE re, VALUE str, VALUE pos, VALUE anchored)
{
    struct re_registers regs;
    struct search_args a;
    OnigPosition r;
    VALUE ret;
    (void)self;
    StringValue(str);
    a.pos = NUM2LONG(pos);
    a.anchored = RTEST(anchored);
    onig_region_init(&regs);
    r = rb_reg_onig_match(re, str, match_fn, &a, &regs);
    if (r == ONIG_MISMATCH) {
        onig_region_free(&regs, 0);
        return Qnil;
    }
    ret = rb_assoc_new(LONG2NUM((long)r), regs_to_ary(&regs));
    onig_region_free(&regs, 0);
    return ret;
}

/* Re.scan(re, str) -> every match's [beg, end], found by onig_search
 * on the prepared buffer, stepping as strscan's scan_until would. */
static VALUE
re_scan(VALUE self, VALUE re, VALUE str)
{
    regex_t *reg;
    struct re_registers regs;
    const UChar *s, *e;
    long pos = 0;
    VALUE ary = rb_ary_new();
    (void)self;
    StringValue(str);
    reg = rb_reg_prepare_re(re, str);
    s = (const UChar *)RSTRING_PTR(str);
    e = s + RSTRING_LEN(str);
    onig_region_init(&regs);
    while (pos <= RSTRING_LEN(str)) {
        OnigPosition r = onig_search(reg, s, e, s + pos, e, &regs, ONIG_OPTION_NONE);
        if (r < 0) break;
        rb_ary_push(ary, rb_assoc_new(LONG2NUM((long)regs.beg[0]), LONG2NUM((long)regs.end[0])));
        pos = regs.end[0] > regs.beg[0] ? regs.end[0] : regs.end[0] + 1;
    }
    onig_region_free(&regs, 0);
    return ary;
}

static int
name_i(const UChar *name, const UChar *name_end, int ngroups, int *groups, regex_t *reg, void *arg)
{
    VALUE ary = (VALUE)arg;
    VALUE nums = rb_ary_new();
    int i;
    (void)reg;
    for (i = 0; i < ngroups; i++) rb_ary_push(nums, INT2FIX(groups[i]));
    rb_ary_push(ary, rb_assoc_new(rb_str_new((const char *)name, name_end - name), nums));
    return 0;
}

/* Re.names(re) -> [[name, [group numbers]], ...], captures count */
static VALUE
re_names(VALUE self, VALUE re)
{
    VALUE ary = rb_ary_new();
    regex_t *reg = RREGEXP_PTR(re);
    (void)self;
    onig_foreach_name(reg, name_i, (void *)ary);
    return rb_ary_new3(3, ary, INT2FIX(onig_number_of_names(reg)), INT2FIX(onig_number_of_captures(reg)));
}

/* Re.backref(re, str, name) -> the group number `name` refers to after
 * a search, as strscan's [] does for a String key. */
static VALUE
re_backref(VALUE self, VALUE re, VALUE str, VALUE name)
{
    struct re_registers regs;
    struct search_args a = {0, 0};
    OnigPosition r;
    int num;
    (void)self;
    StringValue(str);
    StringValue(name);
    onig_region_init(&regs);
    r = rb_reg_onig_match(re, str, match_fn, &a, &regs);
    if (r == ONIG_MISMATCH) {
        onig_region_free(&regs, 0);
        return Qnil;
    }
    num = onig_name_to_backref_number(RREGEXP_PTR(re), (const UChar *)RSTRING_PTR(name),
                                      (const UChar *)RSTRING_END(name), &regs);
    onig_region_free(&regs, 0);
    return INT2FIX(num);
}

/* Re.region_copy(re, str) -> the copied registers, via rb_reg_region_copy */
static VALUE
re_region_copy(VALUE self, VALUE re, VALUE str)
{
    struct re_registers regs, copy;
    struct search_args a = {0, 0};
    VALUE ret;
    (void)self;
    StringValue(str);
    onig_region_init(&regs);
    onig_region_init(&copy);
    if (rb_reg_onig_match(re, str, match_fn, &a, &regs) == ONIG_MISMATCH) {
        onig_region_free(&regs, 0);
        return Qnil;
    }
    if (rb_reg_region_copy(&copy, &regs)) rb_memerror();
    onig_region_clear(&regs);
    ret = rb_ary_new3(3, regs_to_ary(&copy), INT2FIX(regs.num_regs), onig_region_memsize(&copy) > sizeof(copy) ? Qtrue : Qfalse);
    onig_region_free(&regs, 0);
    onig_region_free(&copy, 0);
    return ret;
}

/* Re.memsearch(needle, hay) -> rb_memsearch's byte offset */
static VALUE
re_memsearch(VALUE self, VALUE needle, VALUE hay)
{
    (void)self;
    StringValue(needle);
    StringValue(hay);
    return LONG2NUM(rb_memsearch(RSTRING_PTR(needle), RSTRING_LEN(needle), RSTRING_PTR(hay), RSTRING_LEN(hay), rb_enc_get(hay)));
}

/* Re.misc(src) -> the small helpers: rb_reg_regcomp, rb_reg_quote,
 * rb_reg_options, rb_reg_match / nth_match, rb_long2int */
static VALUE
re_misc(VALUE self, VALUE src)
{
    VALUE re = rb_reg_regcomp(src);
    VALUE q = rb_reg_quote(src);
    VALUE md = rb_funcall(re, rb_intern("match"), 1, rb_str_new_cstr("xx ab12 yy"));
    (void)self;
    return rb_ary_new3(6, re, q, INT2FIX(rb_reg_options(re)),
                       rb_reg_nth_match(0, md), rb_reg_nth_match(1, md), INT2FIX(rb_long2int(123456L)));
}

/* Re.mbclen(str) -> rb_enc_precise_mbclen over every byte offset of str,
 * decoded through the MBCLEN_* / ONIGENC_MBCLEN_* macros: [:found, len],
 * [:invalid] or [:needmore, n]. The same answers on CRuby and monoruby. */
static VALUE
re_mbclen(VALUE self, VALUE str)
{
    rb_encoding *enc = rb_enc_get(str);
    const char *p = RSTRING_PTR(str), *e = p + RSTRING_LEN(str);
    VALUE out = rb_ary_new();
    (void)self;
    for (; p < e; p++) {
        int r = rb_enc_precise_mbclen(p, e, enc);
        VALUE item;
        if (MBCLEN_CHARFOUND_P(r))
            item = rb_ary_new3(2, ID2SYM(rb_intern("found")), INT2FIX(MBCLEN_CHARFOUND_LEN(r)));
        else if (MBCLEN_INVALID_P(r))
            item = rb_ary_new3(1, ID2SYM(rb_intern("invalid")));
        else if (ONIGENC_MBCLEN_NEEDMORE_P(r))
            item = rb_ary_new3(2, ID2SYM(rb_intern("needmore")), INT2FIX(ONIGENC_MBCLEN_NEEDMORE_LEN(r)));
        else
            item = Qnil;
        rb_ary_push(out, item);
    }
    rb_ary_push(out, INT2FIX(rb_enc_precise_mbclen(e, e, enc)));
    return out;
}

static VALUE
re_long2int_overflow(VALUE self)
{
    (void)self;
    return INT2FIX(rb_long2int(1L << 40));
}

void
Init_capi_re(void)
{
    mRe = rb_define_module("CapiRe");
    rb_define_singleton_method(mRe, "run", re_run, 4);
    rb_define_singleton_method(mRe, "scan", re_scan, 2);
    rb_define_singleton_method(mRe, "names", re_names, 1);
    rb_define_singleton_method(mRe, "backref", re_backref, 3);
    rb_define_singleton_method(mRe, "region_copy", re_region_copy, 2);
    rb_define_singleton_method(mRe, "memsearch", re_memsearch, 2);
    rb_define_singleton_method(mRe, "misc", re_misc, 1);
    rb_define_singleton_method(mRe, "mbclen", re_mbclen, 1);
    rb_define_singleton_method(mRe, "long2int_overflow", re_long2int_overflow, 0);
    rb_define_const(mRe, "OLD", INT2FIX(1));
    rb_deprecate_constant(mRe, "OLD");
}
