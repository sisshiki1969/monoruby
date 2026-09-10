/* config.h for the vendored libxslt / libexslt (1.1.43) on unix (Linux
 * glibc / musl, macOS): what `configure` would produce for the feature set
 * in build.rs (no crypto, no plugins), written by hand so the build needs
 * neither autotools nor cmake. It lives in its own include root, apart
 * from libxml2's config.h. */
#ifndef LIBXSLT_MONORUBY_CONFIG_H
#define LIBXSLT_MONORUBY_CONFIG_H

#define HAVE_CLOCK_GETTIME 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GMTIME_R 1
#define HAVE_LOCALTIME_R 1
#define HAVE_LOCALE_H 1
/* strxfrm_l: <locale.h> on glibc / musl (with _GNU_SOURCE), <xlocale.h>
 * on Darwin. Selects the POSIX locale support of xsltlocale.c. */
#define HAVE_STRXFRM_L 1
#ifdef __APPLE__
#define HAVE_XLOCALE_H 1
#endif
#define HAVE_SNPRINTF 1
#define HAVE_STAT 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
#define HAVE_VSNPRINTF 1
#define PACKAGE "libxslt"
#define VERSION "1.1.43"

#endif
