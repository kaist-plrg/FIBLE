#include <caml/mlvalues.h>
#include<fcntl.h>

CAMLprim value unix_getfl(value fd)
{
  int flags = fcntl(Int_val(fd), F_GETFL, 0);
  return Val_int(flags);
}