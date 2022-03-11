/* ccache-wrapper.c - automagically invoking ccache.
 * Copyright (C) 2022 Zhengyi Fu <tsingyat@outlook.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#define _GNU_SOURCE

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <limits.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>

extern char **environ;

int execl(const char *pathname, const char *arg, ...
                       /* (char  *) NULL */);
int execlp(const char *file, const char *arg, ...
                       /* (char  *) NULL */);
int execle(const char *pathname, const char *arg, ...
                       /*, (char *) NULL, char *const envp[] */);
int execv(const char *pathname, char *const argv[]);
int execvp(const char *file, char *const argv[]);
int execvpe(const char *file, char *const argv[],
                       char *const envp[]);


int execve(const char *path, char *const argv[], char *const envp[]);

int fexecve(int fd, char *const argv[], char *const envp[]);

int execveat(int dirfd, const char *pathname,
                    char *const argv[], char *const envp[],
                    int flags);
/* Implementation */

static int _execve(const char *path, char *const argv[], char *const envp[]);
static int _execvpe(const char *path, char *const argv[], char *const envp[]);
static int _fexecve(int fd, char *const argv[], char *const envp[]);

static int enable_debug = -1;

static bool debug_enabled()
{
	if (enable_debug == -1) {
		const char *tmp = getenv("CCACHE_WRAPPER_DEBUG");
		if (tmp && atoi(tmp) != 0)
			enable_debug = 1;
		else
			enable_debug = 0;
	}
	return enable_debug;
}

#ifdef __GNUC__
__attribute__((format(printf, 1, 2)))
#endif
static void debug_printf(const char *fmt, ...)
{
	if (debug_enabled()) {
		va_list ap;
		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
	}
}

static void debug_print_string_array(char *const arr[])
{
	char *const *p = arr;
	bool first = true;

	if (!arr || !debug_enabled())
		return;

	fprintf(stderr, "[");
	while (*p) {
		if (!first)
			fprintf(stderr, ", ");
		first = false;
		fprintf(stderr, "\"%s\"", *p);
		++p;
	}
	fprintf(stderr, "]");
}

static void debug_print_variadic_strings(va_list *ap)
{
	bool first = true;
	const char *s;

	do {
		if (!first)
			debug_printf(", ");
		first = false;

		s = va_arg(*ap, const char *);

		if (s != NULL)
			debug_printf("\"%s\"", s);
		else
			debug_printf("NULL");
	} while (s != NULL);
}

struct flag_name {
	int flag;
	const char *name;
};
#define FLAG_NAME(Flag) { .flag = Flag, .name = #Flag }

static const struct flag_name known_flags[] = {
	FLAG_NAME(AT_EMPTY_PATH),
	FLAG_NAME(AT_SYMLINK_NOFOLLOW),
	{ 0, NULL },
};

static void debug_print_at_flags(int flags)
{
	bool first = true;
	const struct flag_name *iter;

	for (iter = known_flags; iter->flag != 0; ++iter) {
		if (flags & iter->flag) {
			if (!first)
				debug_printf("|");
			first = false;
			debug_printf("%s", iter->name);
		}
	}
	if (first)
		debug_printf("0");
}

static void debug_trace(const char *function, const char *fmt, ...)
{
	bool first = true;

	int i;
	const char *s;
	char *const *a;
	va_list *v;

	va_list ap;
	va_start(ap, fmt);

	debug_printf("%s(", function);
	while (*fmt) {
		if (!first)
			debug_printf(", ");
		first = false;

		switch (*fmt) {
		case 'i':
			i = va_arg(ap, int);
			debug_printf("%i", i);
			break;
		case 's':
			s = va_arg(ap, const char *);
			debug_printf("%s", s);
			break;
		case 'a':
			a = va_arg(ap, char *const *);
			debug_print_string_array(a);
			break;
		case 'v':
			v = va_arg(ap, va_list *);
			debug_print_variadic_strings(v);
			if (fmt[1] == 'e') {
				debug_print_variadic_strings(v);
				++fmt;
			}
			break;
		case 'f':
			i = va_arg(ap, int);
			debug_print_at_flags(i);
			break;
		default:
			debug_printf("?");
			break;
		}

		++fmt;
	}
	debug_printf(")\n");
	va_end(ap);
}

#define debug_trace_v(Fmt, Last, ...) \
	do {\
		va_list _va; \
		va_start(_va, Last); \
		debug_trace(__func__, Fmt, __VA_ARGS__, &_va); \
		va_end(_va); \
	} while (0)


static const char *compiler_names[] = {
	"cc",
	"c++",
	"gcc",
	"g++",
	"clang",
	"clang++",
	NULL
};

static const char *basename_ref(const char *pathname)
{
	size_t i = strlen(pathname);
	while (i != 0) {
		if (pathname[i - 1] == '/')
			return &pathname[i];
		--i;
	}
	return pathname;
}

static bool is_compiler(const char *file)
{
	const char *base = basename_ref(file);
	const char **compiler;

	for (compiler = compiler_names; *compiler != NULL; ++compiler) {
		if (strcmp(*compiler, base) == 0) {
			debug_printf("%s is compiler\n", base);
			return true;
		}
	}
	debug_printf("%s is not compiler\n", base);
	return false;
}

static bool is_ccache()
{
	char buffer[PATH_MAX] = { 0 };
	ssize_t len = readlink("/proc/self/exe", buffer, sizeof(buffer) - 1);
	const char *base;
	if (len < 0) {
		debug_printf("readlink(\"/proc/self/exe\", {}, %ld) == %d (%s)",
				(long)(sizeof(buffer) - 1), errno, strerror(errno));
		return false;
	}
	base = basename_ref(buffer);
	return strcmp(base, "ccache") == 0;
}

static size_t get_args(va_list *ap, char **args, size_t args_max)
{
	size_t i = 0;
	char *a;

	do {
		a = va_arg(*ap, char *);
		args[i++] = a;
	} while (i < args_max && a != NULL);
	return i;
}

#define ARGS_MAX 8196

int execl(const char *pathname, const char *arg, ...)
{
	va_list ap;
	char *argv[ARGS_MAX];

	debug_trace_v("ssv", arg, pathname, arg);

	argv[0] = (char *)pathname;
	va_start(ap, arg);
	get_args(&ap, argv + 1, ARGS_MAX - 1);
	va_end(ap);

	return _execve(pathname, argv, environ);
}

int execlp(const char *file, const char *arg, ...)
{
	va_list ap;
	char *argv[ARGS_MAX];

	debug_trace_v("ssv", arg, file, arg);

	argv[0] = (char *)arg;
	va_start(ap, arg);
	va_start(ap, arg);
	get_args(&ap, argv + 1, ARGS_MAX - 1);
	va_end(ap);

	return _execvpe(file, argv, environ);
}

int execle(const char *pathname, const char *arg, ...)
{
	va_list ap;
	char *argv[ARGS_MAX];
	char *const* envp;

	debug_trace_v("ssv", arg, pathname, arg);

	argv[0] = (char *)pathname;
	va_start(ap, arg);
	va_start(ap, arg);
	get_args(&ap, argv + 1, ARGS_MAX - 1);
	envp = va_arg(ap, char *const *);
	va_end(ap);

	return _execve(pathname, argv, envp);
}

int execv(const char *pathname, char *const argv[])
{
	debug_trace(__func__, "sa", pathname, argv);
	return _execve(pathname, argv, environ);
}

int execvp(const char *file, char *const argv[])
{
	debug_trace(__func__, "sa", file, argv);
	return _execvpe(file, argv, environ);
}

int execvpe(const char *pathname, char *const argv[], char *const envp[])
{
	debug_trace(__func__, "saa", pathname, argv, envp);
	return _execvpe(pathname, argv, envp);
}

typedef int (*execve_fn)(const char *, char *const [], char *const []);

static int _execvpe(const char *pathname, char *const argv[], char *const envp[])
{
	char *new_argv[ARGS_MAX];
	char ccache_path[PATH_MAX] = { 0 };
	char *tmp;
	int prefix_len;
	int i;
	execve_fn real_execve;


	dlerror();
	real_execve = dlsym(RTLD_NEXT, "execvpe");
	if (!real_execve) {
		char const *err = dlerror();
		debug_printf("dlsym(RTLD_NEXT, \"execpve\") = NULL (%s)\n", err);		
		errno = EINVAL;
		return -1;
	}

	if (!is_compiler(pathname) || is_ccache())
		return real_execve(pathname, argv, envp);

	tmp = getenv("CCACHE_PATH");
	if (!tmp) {
		new_argv[0] = "ccache";
		prefix_len = 1;
	} else {
		strncpy(ccache_path, tmp, sizeof(ccache_path) - 1);
		new_argv[0] = ccache_path;
		prefix_len = 1;
	}

	i = 0;
	do {
		tmp = argv[i];
		if (i == 0)
			tmp = (char *)pathname;
		new_argv[prefix_len + i] = tmp;
		++i;

		if (i >= ARGS_MAX) {
			errno = EINVAL;
			return -1;
		}
	} while (tmp != NULL);

	debug_printf("pid = %ld ", (long)getpid());
	for (i = 0; new_argv[i] != NULL; ++i) {
		debug_printf("argv[%i] = %s ", i, new_argv[i]);
	}
	debug_printf("\n");
	return real_execve(new_argv[0], new_argv, envp);
}

int execve(const char *pathname, char *const argv[], char *const envp[])
{
	debug_trace(__func__, "saa", pathname, argv, envp);
	return _execve(pathname, argv, envp);
}

static int _execve(const char *pathname, char *const argv[], char *const envp[])
{
	char *new_argv[ARGS_MAX];
	char ccache_path[PATH_MAX] = { 0 };
	char *tmp;
	int prefix_len;
	int i;
	execve_fn real_execve;

	dlerror();
	real_execve = dlsym(RTLD_NEXT, "execve");
	if (!real_execve) {
		char const *err = dlerror();
		debug_printf("dlsym(RTLD_NEXT, \"execve\") = NULL (%s)\n", err);		
		errno = EINVAL;
		return -1;
	}

	if (!is_compiler(pathname) || is_ccache())
		return real_execve(pathname, argv, envp);

	tmp = getenv("CCACHE_PATH");
	if (!tmp) {
		new_argv[0] = "/usr/bin/env";
		new_argv[1] = "ccache";
		prefix_len = 2;
	} else {
		strncpy(ccache_path, tmp, sizeof(ccache_path) - 1);
		new_argv[0] = ccache_path;
		prefix_len = 1;
	}

	i = 0;
	do {
		tmp = argv[i];
		if (i == 0)
			tmp = (char *)pathname;
		new_argv[prefix_len + i] = tmp;
		++i;

		if (i >= ARGS_MAX) {
			errno = EINVAL;
			return -1;
		}
	} while (tmp != NULL);

	debug_printf("pid = %ld ", (long)getpid());
	for (i = 0; new_argv[i] != NULL; ++i) {
		debug_printf("argv[%i] = %s ", i, new_argv[i]);
	}
	debug_printf("\n");
	return real_execve(new_argv[0], new_argv, envp);
}

int fexecve(int fd, char *const argv[], char *const envp[])
{
	debug_trace(__func__, "iaa", fd, argv, envp);
	return _fexecve(fd, argv, envp);
}

typedef int (*fexecve_fn)(int, char *const *, char *const *);

static int _fexecve(int fd, char *const argv[], char *const envp[])
{
	char buffer[sizeof("/proc/self/fd/9999999999")] = { 0 };
	char pathname[PATH_MAX] = { 0 };
	fexecve_fn real_fexecve = NULL;

	if (is_ccache())
		goto call_real_fexecve;

	snprintf(buffer, sizeof(buffer) - 1, "/proc/self/fd/%d", fd);
	if (readlink(buffer, pathname, sizeof(pathname) - 1) < 0)
		return -1;

	if (access(pathname, X_OK) < 0) {
		debug_printf("fexecve(%d, argv, envp): ", fd);
		debug_printf("/proc/self/fd/%d -> %s\n", fd, pathname);
		errno = EACCES;
		return -1;
	}

	if (!is_compiler(pathname))
		goto call_real_fexecve;

	return _execve(pathname, argv, envp);

call_real_fexecve:
	dlerror();
	real_fexecve = dlsym(RTLD_NEXT, "fexecve");
	debug_printf("dlsym(RTLD_NEXT, \"fexecve\") = %p", real_fexecve);
	if (!real_fexecve) {
		const char *err = dlerror();
		if (err) {
			debug_printf("(%s)", err);
		}
	}
	debug_printf("\n");
	if (!real_fexecve) {
		errno = ENOTSUP;
		return -1;
	}

	return real_fexecve(fd, argv, envp);
}

typedef int (*execveat_fn)(int, const char *, 
		char *const*, char *const *, int);

int execveat(int dirfd, const char *pathname,
                    char *const argv[], char *const envp[],
                    int flags)
{
	int oflags;
	int fd;
	int err;

	debug_trace(__func__, "isaaf", dirfd, pathname, argv, envp, flags);

	if (flags & AT_EMPTY_PATH && pathname == NULL)
		return _fexecve(dirfd, argv, envp);

	oflags = O_PATH | O_CLOEXEC;
	if (flags & AT_SYMLINK_NOFOLLOW)
		oflags |= O_NOFOLLOW;
	fd = openat(dirfd, pathname, oflags);
	if (fd < 0)
		return -1;
	
	_fexecve(fd, argv, envp);
	err = errno;
	close(fd);
	errno = err;
	return -1;
}

