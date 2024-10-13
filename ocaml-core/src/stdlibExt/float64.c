#define _GNU_SOURCE

#include <math.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

/* Accessing the float part of an OCaml custom block */
#define Float64_val(v) (*((double *) Data_custom_val(v)))

/* Encapsulation of opaque window handles (of type float32 *)
   as OCaml custom blocks. */

static int compare_float64(value v1, value v2)
{
    float f1 = Float64_val(v1);
    float f2 = Float64_val(v2);
    return (f1 > f2) - (f1 < f2);
}

static intnat hash_float64(value v)
{
  long long int ha[2] = {0, 0};
  intnat h = 0;
  *(double *)(&ha) = Float64_val(v);
  h = ha[0];
  return h;
}

static struct custom_operations curses_float64_ops = {
  "fr.inria.caml.float64",
  custom_finalize_default,
  compare_float64,
  hash_float64,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Allocating an OCaml custom block to hold the given float * */
static value alloc_double(double w)
{
  value v = caml_alloc_custom(&curses_float64_ops, sizeof(double), 0, 1);
  Float64_val(v) = w;
  return v;
}

CAMLprim value caml_float64_equal(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_bool(Float64_val(v1) == Float64_val(v2)));
}

CAMLprim value caml_float64_compare(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_int(compare_float64(v1, v2)));
}

CAMLprim value caml_float64_of_bytes(value s)
{
  CAMLparam1(s);
  CAMLreturn(alloc_double(*(double *) Bytes_val(s)));
}

CAMLprim value caml_float64_to_bytes(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(double));
  *(double *) Bytes_val(s) = Float64_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float64_of_string(value s)
{
  CAMLparam1(s);
  CAMLreturn (alloc_double(*(double *) String_val(s)));
}

CAMLprim value caml_float64_to_string(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(double));
  *(double *) String_val(s) = Float64_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float64_show(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  char *sp;
  int res = asprintf(&sp, "%lf", Float64_val(w));
  if (res == -1) caml_failwith("caml_float64_show");
  s = caml_copy_string(sp);
  free(sp);
  CAMLreturn(s);
}

CAMLprim value caml_float64_read(value s)
{
  CAMLparam1(s);
  CAMLlocal1(v);
  double f;
  int res = sscanf(String_val(s), "%lf", &f);
  if (res != 1) caml_failwith("caml_float64_read");
  v = alloc_double(f);
  CAMLreturn(v);
}

CAMLprim value caml_float64_neg(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(-Float64_val(v)));
}

CAMLprim value caml_float64_abs(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(fabs(Float64_val(v))));
}

CAMLprim value caml_float64_sqrt(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(sqrt(Float64_val(v))));
}

CAMLprim value caml_float64_ceil(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(ceil(Float64_val(v))));
}

CAMLprim value caml_float64_floor(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(floor(Float64_val(v))));
}

CAMLprim value caml_float64_round(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_double(floor(Float64_val(v))));
}

CAMLprim value caml_float64_is_nan(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_bool(isnan(Float64_val(v))));
}

CAMLprim value caml_float64_trunc(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_int64((int64_t)(trunc(Float64_val(v)))));
}

CAMLprim value caml_float64_add(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_double(Float64_val(v1) + Float64_val(v2)));
}

CAMLprim value caml_float64_sub(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_double(Float64_val(v1) - Float64_val(v2)));
}

CAMLprim value caml_float64_mul(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_double(Float64_val(v1) * Float64_val(v2)));
}

CAMLprim value caml_float64_div(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_double(Float64_val(v1) / Float64_val(v2)));
}