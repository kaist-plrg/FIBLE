#define _GNU_SOURCE

#include <math.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

/* Accessing the float part of an OCaml custom block */
#define Float80_val(v) (*((long double *) Data_custom_val(v)))

/* Encapsulation of opaque window handles (of type float32 *)
   as OCaml custom blocks. */

static int compare_float80(value v1, value v2)
{
    float f1 = Float80_val(v1);
    float f2 = Float80_val(v2);
    return (f1 > f2) - (f1 < f2);
}

static intnat hash_float80(value v)
{
  long long int ha[2] = {0, 0};
  intnat h = 0;
  *(long double *)(&ha) = Float80_val(v);
  h = ha[0];
  return h;
}

static struct custom_operations curses_float80_ops = {
  "fr.inria.caml.float80",
  custom_finalize_default,
  compare_float80,
  hash_float80,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Allocating an OCaml custom block to hold the given float * */
static value alloc_longdouble(long double w)
{
  value v = caml_alloc_custom(&curses_float80_ops, sizeof(long double), 0, 1);
  Float80_val(v) = w;
  return v;
}

CAMLprim value caml_float80_equal(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_bool(Float80_val(v1) == Float80_val(v2)));
}

CAMLprim value caml_float80_compare(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_int(compare_float80(v1, v2)));
}

CAMLprim value caml_float80_of_bytes(value s)
{
  CAMLparam1(s);
  CAMLreturn(alloc_longdouble(*(long double *) Bytes_val(s)));
}

CAMLprim value caml_float80_to_bytes(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(long double));
  *(long double *) Bytes_val(s) = Float80_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float80_of_string(value s)
{
  CAMLparam1(s);
  CAMLreturn (alloc_longdouble(*(long double *) String_val(s)));
}

CAMLprim value caml_float80_to_string(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(long double));
  *(long double *) String_val(s) = Float80_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float80_show(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  char *sp;
  int res = asprintf(&sp, "%Lf", Float80_val(w));
  if (res == -1) caml_failwith("caml_float80_show");
  s = caml_copy_string(sp);
  free(sp);
  CAMLreturn(s);
}

CAMLprim value caml_float80_read(value s)
{
  CAMLparam1(s);
  CAMLlocal1(v);
  long double f;
  int res = sscanf(String_val(s), "%Lf", &f);
  if (res != 1) caml_failwith("caml_float80_read");
  v = alloc_longdouble(f);
  CAMLreturn(v);
}


CAMLprim value caml_float80_neg(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(-Float80_val(v)));
}

CAMLprim value caml_float80_abs(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(fabsl(Float80_val(v))));
}

CAMLprim value caml_float80_sqrt(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(sqrtl(Float80_val(v))));
}

CAMLprim value caml_float80_ceil(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(ceill(Float80_val(v))));
}

CAMLprim value caml_float80_floor(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(floorl(Float80_val(v))));
}

CAMLprim value caml_float80_round(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_longdouble(roundl(Float80_val(v))));
}

CAMLprim value caml_float80_is_nan(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_bool(isnan(Float80_val(v))));
}

CAMLprim value caml_float80_trunc(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_int64((int64_t)(truncl(Float80_val(v)))));
}

CAMLprim value caml_float80_add(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_longdouble(Float80_val(v1) + Float80_val(v2)));
}

CAMLprim value caml_float80_sub(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_longdouble(Float80_val(v1) - Float80_val(v2)));
}

CAMLprim value caml_float80_mul(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_longdouble(Float80_val(v1) * Float80_val(v2)));
}

CAMLprim value caml_float80_div(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_longdouble(Float80_val(v1) / Float80_val(v2)));
}