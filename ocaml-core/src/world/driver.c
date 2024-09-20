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

CAMLprim value unix_read (value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  int64_t ret = read(Int_val(fd), Bytes_val(buf), Int64_val(len));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(caml_copy_int64(ret));
}

CAMLprim value unix_write(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  int64_t ret = write(Int_val(fd), String_val(buf), Int64_val(len));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(caml_copy_int64(ret));
}

CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int ret = open(String_val(path), Int_val(flags), Int_val(perm));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_close(value fd)
{
  CAMLparam1(fd);
  int ret = close(Int_val(fd));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_stat(value path, value buf)
{
  CAMLparam2(path, buf);
  int ret = stat(String_val(path), (struct stat *)Bytes_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_fstat(value fd, value buf)
{
  CAMLparam2(fd, buf);
  int ret = fstat(Int_val(fd), (struct stat *)Bytes_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_lstat(value path, value buf)
{
  CAMLparam2(path, buf);
  int ret = lstat(String_val(path), (struct stat *)Bytes_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_tcgets(value fd, value buf)
{
  CAMLparam2(fd, buf);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = ioctl(Int_val(fd), TCGETS, (struct termios *)Bytes_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_tcsets(value fd, value buf)
{
  CAMLparam2(fd, buf);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = ioctl(Int_val(fd), TCSETS, (const struct termios *)String_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_tcsetsw(value fd, value buf)
{
  CAMLparam2(fd, buf);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = ioctl(Int_val(fd), TCSETSW, (const struct termios *)String_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_tiocgwinsz(value fd, value buf)
{
  CAMLparam2(fd, buf);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = ioctl(Int_val(fd), TIOCGWINSZ, (struct winsize *)Bytes_val(buf));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_ioctl (value fd, value request, value arg)
{
  CAMLparam3(fd, request, arg);
  int ret = ioctl(Int_val(fd), Int_val(request), Int64_val(arg));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_dupfd(value fd, value minfd)
{
  CAMLparam2(fd, minfd);
  int ret = fcntl(Int_val(fd), F_DUPFD, Int_val(minfd));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_getfl(value fd)
{
  CAMLparam1(fd);
  int flags = fcntl(Int_val(fd), F_GETFL, 0);
  if (flags == -1) {
    flags = -errno;
  }
  CAMLreturn(Val_int(flags));
}

CAMLprim value unix_setfl(value fd, value flags)
{
  CAMLparam2(fd, flags);
  int ret = fcntl(Int_val(fd), F_SETFL, Int_val(flags));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}


CAMLprim value unix_getfd(value fd)
{
  CAMLparam1(fd);
  int flags = fcntl(Int_val(fd), F_GETFD, 0);
  if (flags == -1) {
    flags = -errno;
  }
  CAMLreturn(Val_int(flags));
}

CAMLprim value unix_setfd(value fd, value flags)
{
  CAMLparam2(fd, flags);
  int ret = fcntl(Int_val(fd), F_SETFD, Int_val(flags));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_getown(value fd)
{
  CAMLparam1(fd);
  int pid = fcntl(Int_val(fd), F_GETOWN, 0);
  if (pid == -1) {
    pid = -errno;
  }
  CAMLreturn(Val_int(pid));
}

CAMLprim value unix_setown(value fd, value pid)
{
  CAMLparam2(fd, pid);
  int ret = fcntl(Int_val(fd), F_SETOWN, Int_val(pid));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_getdents64(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = syscall(SYS_getdents, Int_val(fd), Bytes_val(buf), Int_val(len));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_fadvise64(value fd, value offset, value len, value advice)
{
  CAMLparam4(fd, offset, len, advice);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = posix_fadvise(Int_val(fd), Int64_val(offset), Int64_val(len), Int_val(advice));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_getcwd(value buf, value len)
{
  CAMLparam2(buf, len);
  int retv = strlen((char *)Bytes_val(buf)) + 1;
  char *ret = getcwd((char *)Bytes_val(buf), Int_val(len));;
  if (ret == NULL) {
    retv = -errno;
  }
  CAMLreturn(Val_int(retv));
}

CAMLprim value unix_chroot(value path)
{
  CAMLparam1(path);
  int ret = chroot(String_val(path));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_openat(value dirfd, value path, value flags, value perm)
{
  CAMLparam4(dirfd, path, flags, perm);
  int ret = openat(Int_val(dirfd), String_val(path), Int_val(flags), Int_val(perm));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_newfstatat(value dirfd, value path, value buf, value flags)
{
  CAMLparam4(dirfd, path, buf, flags);
  int ret = fstatat(Int_val(dirfd), String_val(path), (struct stat *)Bytes_val(buf), Int_val(flags));
  if (ret == -1) {
    ret = -errno;
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_fd_is_valid (value fd)
{
  CAMLparam1(fd);
  CAMLreturn(Val_bool(fcntl(Int_val(fd), F_GETFD) != -1));
}