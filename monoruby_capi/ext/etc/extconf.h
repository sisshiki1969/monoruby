/*
 * extconf.h for CRuby 4.0.9's ext/etc built against monoruby's C API
 * layer (build.rs, `build_capi_extensions`): what its extconf.rb defines
 * on Linux (glibc) and on macOS, plus the few `config.h` answers etc.c
 * reads that the layer's own `ruby/config.h` does not give.
 */
#ifndef EXTCONF_H
#define EXTCONF_H
#define HAVE_SYS_UTSNAME_H 1
#define HAVE_UNAME 1
#define HAVE_GETLOGIN 1
#define HAVE_GETPWENT 1
#define HAVE_STRUCT_PASSWD_PW_GECOS 1
#define HAVE_STRUCT_PASSWD_PW_PASSWD 1
#if defined(__APPLE__)
#define HAVE_STRUCT_PASSWD_PW_CHANGE 1
#define HAVE_STRUCT_PASSWD_PW_CLASS 1
#define HAVE_STRUCT_PASSWD_PW_EXPIRE 1
#endif
#define HAVE_GETGRENT 1
#define HAVE_STRUCT_GROUP_GR_PASSWD 1
#define HAVE_SYSCONF 1
#define HAVE_CONFSTR 1
#define HAVE_FPATHCONF 1
#define HAVE_RB_DEPRECATE_CONSTANT 1
#define HAVE_RB_IO_DESCRIPTOR 1
/* config.h's */
#if defined(__linux__)
#define HAVE_SCHED_GETAFFINITY 1
#endif
#endif
