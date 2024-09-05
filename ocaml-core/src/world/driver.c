#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

CAMLprim value unix_getfl(value fd)
{
  int flags = fcntl(Int_val(fd), F_GETFL, 0);
  return Val_int(flags);
}

CAMLprim value unix_read (value fd, value buf, value len)
{
  int64_t ret = read(Int_val(fd), Bytes_val(buf), Int64_val(len));
  return caml_copy_int64(ret);
}

CAMLprim value unix_write(value fd, value buf, value len)
{
  int64_t ret = write(Int_val(fd), String_val(buf), Int64_val(len));
  return caml_copy_int64(ret);
}

CAMLprim value unix_open(value path, value flags, value perm)
{
  int ret = open(String_val(path), Int_val(flags), Int_val(perm));
  return Val_int(ret);
}

CAMLprim value unix_close(value fd)
{
  int ret = close(Int_val(fd));
  return Val_int(ret);
}

CAMLprim value unix_ioctl (value fd, value request, value arg)
{
  int ret = ioctl(Int_val(fd), Int_val(request), Int64_val(arg));
  return Val_int(ret);
}

CAMLprim value unix_fadvise(value fd, value offset, value len, value advice)
{
#ifdef __APPLE__
  return 0;
#else
  int ret = posix_fadvise(Int_val(fd), Int64_val(offset), Int64_val(len), Int_val(advice));
  return Val_int(ret);
#endif
}

CAMLprim value unix_fd_is_valid (value fd)
{
  return Val_bool(fcntl(Int_val(fd), F_GETFD) != -1);
}