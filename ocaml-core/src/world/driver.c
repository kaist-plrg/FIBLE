#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLprim value unix_getfl(value fd)
{
  CAMLparam1(fd);
  int flags = fcntl(Int_val(fd), F_GETFL, 0);
  CAMLreturn(Val_int(flags));
}

CAMLprim value unix_read (value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  int64_t ret = read(Int_val(fd), Bytes_val(buf), Int64_val(len));
  CAMLreturn(caml_copy_int64(ret));
}

CAMLprim value unix_write(value fd, value buf, value len)
{
  CAMLparam3(fd, buf, len);
  int64_t ret = write(Int_val(fd), String_val(buf), Int64_val(len));
  CAMLreturn(caml_copy_int64(ret));
}

CAMLprim value unix_open(value path, value flags, value perm)
{
  CAMLparam3(path, flags, perm);
  int ret = open(String_val(path), Int_val(flags), Int_val(perm));
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_close(value fd)
{
  CAMLparam1(fd);
  int ret = close(Int_val(fd));
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_ioctl (value fd, value request, value arg)
{
  CAMLparam3(fd, request, arg);
  int ret = ioctl(Int_val(fd), Int_val(request), Int64_val(arg));
  CAMLreturn(Val_int(ret));
}

CAMLprim value unix_fadvise(value fd, value offset, value len, value advice)
{
  CAMLparam4(fd, offset, len, advice);
#ifdef __APPLE__
  CAMLreturn(Val_int(0));
#else
  int ret = posix_fadvise(Int_val(fd), Int64_val(offset), Int64_val(len), Int_val(advice));
  CAMLreturn(Val_int(ret));
#endif
}

CAMLprim value unix_fd_is_valid (value fd)
{
  CAMLparam1(fd);
  CAMLreturn(Val_bool(fcntl(Int_val(fd), F_GETFD) != -1));
}