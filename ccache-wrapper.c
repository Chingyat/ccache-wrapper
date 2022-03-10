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

static int enable_debug = -1;

#ifdef __GNUC__
__attribute__((format(printf, 1, 2)))
#endif
static void debug_printf(const char *fmt, ...)
{
	if (enable_debug == -1) {
		const char *tmp = getenv("CCACHE_WRAPPER_DEBUG");
		if (tmp && atoi(tmp) != 0)
			enable_debug = 1;
		else
			enable_debug = 0;
	}

	if (enable_debug == 1) {
		va_list ap;
		va_start(ap, fmt);
		vfprintf(stderr, fmt, ap);
		va_end(ap);
	}
}

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

	va_start(ap, arg);
	get_args(&ap, argv, ARGS_MAX);
	va_end(ap);

	return execve(pathname, argv, environ);
}

int execlp(const char *file, const char *arg, ...)
{
	va_list ap;
	char *argv[ARGS_MAX];

	va_start(ap, arg);
	get_args(&ap, argv, ARGS_MAX);
	va_end(ap);

	return execvp(file, argv);
}

int execle(const char *pathname, const char *arg, ...)
{
	va_list ap;
	char *argv[ARGS_MAX];
	char *const* envp;

	va_start(ap, arg);
	get_args(&ap, argv, ARGS_MAX);
	envp = va_arg(ap, char *const *);
	va_end(ap);

	return execve(pathname, argv, envp);
}

int execv(const char *pathname, char *const argv[])
{
	return execve(pathname, argv, environ);
}

int execvp(const char *file, char *const argv[])
{
	return execvpe(file, argv, environ);
}

typedef int (*execve_fn)(const char *, char *const [], char *const []);


int execvpe(const char *pathname, char *const argv[], char *const envp[])
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

typedef int (*fexecve_fn)(int, char *const *, char *const *);

int fexecve(int fd, char *const argv[], char *const envp[])
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

	return execve(pathname, argv, envp);

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

	if (flags & AT_EMPTY_PATH && pathname == NULL)
		return fexecve(dirfd, argv, envp);

	oflags = O_PATH | O_CLOEXEC;
	if (flags & AT_SYMLINK_NOFOLLOW)
		oflags |= O_NOFOLLOW;
	fd = openat(dirfd, pathname, oflags);
	if (fd < 0)
		return -1;
	
	fexecve(fd, argv, envp);
	err = errno;
	close(fd);
	errno = err;
	return -1;
}

