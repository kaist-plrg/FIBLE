#ifndef __APPLE__
#include <asm/termbits.h>
#endif
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#ifdef __APPLE__
#define DO_SYS(sname, ...) caml_failwith("syscall " #sname " not supported on macOS")
#else
#define DO_SYS(sname, ...) ({ \
  CAMLreturn(caml_copy_int64(syscall(SYS_ ## sname, ##__VA_ARGS__))); \
})
#endif

// 0: read
CAMLprim value unix_read (value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (read, Int_val(fd), Bytes_val(buf), Int64_val(len));
}

// 1: write
CAMLprim value unix_write(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (write, Int_val(fd), String_val(buf), Int64_val(len));
}

// 2: open
CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  DO_SYS (open, String_val(path), Int_val(flags), Int_val(perm));
}

// 3: close
CAMLprim value unix_close(value fd)
{
  CAMLparam1(fd);
  DO_SYS (close, Int_val(fd));
}

// 4: stat
CAMLprim value unix_stat(value path, value buf)
{
  CAMLparam2(path, buf);
  DO_SYS (stat, String_val(path), (struct stat *)Bytes_val(buf));
}

// 5: fstat
CAMLprim value unix_fstat(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (fstat, Int_val(fd), (struct stat *)Bytes_val(buf));
}

// 6: lstat
CAMLprim value unix_lstat(value path, value buf)
{
  CAMLparam2(path, buf);
  DO_SYS (lstat, String_val(path), (struct stat *)Bytes_val(buf));
}

// 16-1: ioctl-tcgets
CAMLprim value unix_tcgets(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCGETS, (struct termios *)Bytes_val(buf));
}

// 16-2: ioctl-tcsets
CAMLprim value unix_tcsets(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCSETS, (const struct termios *)String_val(buf));
}

// 16-3: ioctl-tcsetsw
CAMLprim value unix_tcsetsw(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TCSETSW, (const struct termios *)String_val(buf));
}

// 16-4: ioctl-tiocgwinsz
CAMLprim value unix_tiocgwinsz(value fd, value buf)
{
  CAMLparam2(fd, buf);
  DO_SYS (ioctl, Int_val(fd), TIOCGWINSZ, (struct winsize *)Bytes_val(buf));
}

// 72-1: fcntl-f_dupfd
CAMLprim value unix_dupfd(value fd, value minfd)
{
  CAMLparam2(fd, minfd);
  DO_SYS (fcntl, Int_val(fd), F_DUPFD, Int_val(minfd));
}

// 72-2: fcntl-f_getfl
CAMLprim value unix_getfl(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETFL);
}

// 72-3: fcntl-f_setfl
CAMLprim value unix_setfl(value fd, value flags)
{
  CAMLparam2(fd, flags);
  DO_SYS (fcntl, Int_val(fd), F_SETFL, Int_val(flags));
}

// 72-4: fcntl-f_getfd
CAMLprim value unix_getfd(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETFD);
}

// 72-5: fcntl-f_setfd
CAMLprim value unix_setfd(value fd, value flags)
{
  CAMLparam2(fd, flags);
  DO_SYS (fcntl, Int_val(fd), F_SETFD, Int_val(flags));
}

// 72-6: fcntl-f_getown
CAMLprim value unix_getown(value fd)
{
  CAMLparam1(fd);
  DO_SYS (fcntl, Int_val(fd), F_GETOWN);
}

// 72-7: fcntl-f_setown
CAMLprim value unix_setown(value fd, value pid)
{
  CAMLparam2(fd, pid);
  DO_SYS (fcntl, Int_val(fd), F_SETOWN, Int_val(pid));
}

// 79: getcwd
CAMLprim value unix_getcwd(value buf, value len)
{
  CAMLparam2(buf, len);
  DO_SYS (getcwd, Bytes_val(buf), Int_val(len));
}

// 89: readlink
CAMLprim value unix_readlink(value path, value buf, value len)
{
  CAMLparam3(path, buf, len);
  DO_SYS (readlink, String_val(path), Bytes_val(buf), Int_val(len));
}

// 96: gettimeofday
CAMLprim value unix_gettimeofday(value tv, value tz)
{
  CAMLparam2(tv, tz);
  DO_SYS (gettimeofday, (struct timeval *)Bytes_val(tv), (struct timezone *)Bytes_val(tz));
}

// 161: chroot
CAMLprim value unix_chroot(value path)
{
  CAMLparam1(path);
  DO_SYS (chroot, String_val(path));
}

// 217: getdents64
CAMLprim value unix_getdents64(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  DO_SYS (getdents64, Int_val(fd), Bytes_val(buf), Int_val(len));
}

// 221: fadvise64
CAMLprim value unix_fadvise64(value fd, value offset, value len, value advice)
{
  CAMLparam4(fd, offset, len, advice);
  DO_SYS (fadvise64, Int_val(fd), Int64_val(offset), Int64_val(len), Int_val(advice));
}

// 228: clock_gettime
CAMLprim value unix_clock_gettime(value clk_id, value tp)
{
  CAMLparam2(clk_id, tp);
  DO_SYS (clock_gettime, Int64_val(clk_id), (struct timespec *)Bytes_val(tp));
}

// 257: openat
CAMLprim value unix_openat(value dirfd, value path, value flags, value perm)
{
  CAMLparam4(dirfd, path, flags, perm);
  DO_SYS (openat, Int_val(dirfd), String_val(path), Int_val(flags), Int_val(perm));
}

// 262: newfstatat
CAMLprim value unix_newfstatat(value dirfd, value path, value buf, value flags)
{
  CAMLparam4(dirfd, path, buf, flags);
  DO_SYS (newfstatat, Int_val(dirfd), String_val(path), (struct stat *)Bytes_val(buf), Int_val(flags));
}

CAMLprim value unix_fd_is_valid (value fd)
{
  CAMLparam1(fd);
  CAMLreturn(Val_bool(fcntl(Int_val(fd), F_GETFD) != -1));
}
