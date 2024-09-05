#include <caml/mlvalues.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

CAMLprim value unix_getfl(value fd)
{
  int flags = fcntl(Int_val(fd), F_GETFL, 0);
  return Val_int(flags);
}

CAMLprim value unix_write(value fd, value buf, value len)
{
  ssize_t ret = write(Int_val(fd), String_val(buf), Int_val(len));
  return Val_int(ret);
}

CAMLprim value unix_ioctl (value fd, value request, value arg)
{
  int ret = ioctl(Int_val(fd), Int_val(request), Int64_val(arg));
  return Val_int(ret);
}