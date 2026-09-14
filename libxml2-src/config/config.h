/* config.h for the vendored libxml2 (2.13.8) on unix (Linux glibc / musl,
 * macOS): what `configure` would produce for the feature set in build.rs,
 * written by hand so the build needs neither autotools nor cmake. */
#ifndef LIBXML2_MONORUBY_CONFIG_H
#define LIBXML2_MONORUBY_CONFIG_H

#define ATTRIBUTE_DESTRUCTOR __attribute__((destructor))
#define HAVE_ATTRIBUTE_DESTRUCTOR 1
#define HAVE_ARPA_INET_H 1
#define HAVE_DLFCN_H 1
#define HAVE_DLOPEN 1
#define HAVE_FCNTL_H 1
#define HAVE_GETENTROPY 1
#define HAVE_GETTIMEOFDAY 1
#define HAVE_MMAP 1
#define HAVE_MUNMAP 1
#define HAVE_NETDB_H 1
#define HAVE_NETINET_IN_H 1
#define HAVE_POLL_H 1
#define HAVE_PTHREAD_H 1
#define HAVE_STAT 1
#define HAVE_STDINT_H 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_SYS_RANDOM_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#define SUPPORT_IP6 1
#define VERSION "2.13.8"
#define XML_SOCKLEN_T socklen_t
#define XML_THREAD_LOCAL _Thread_local

#endif
