#define _GNU_SOURCE

#include <math.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

/* Accessing the float part of an OCaml custom block */
#define Float32_val(v) (*((float *) Data_custom_val(v)))

/* Encapsulation of opaque window handles (of type float32 *)
   as OCaml custom blocks. */

static int compare_float32(value v1, value v2)
{
    float f1 = Float32_val(v1);
    float f2 = Float32_val(v2);
    return (f1 > f2) - (f1 < f2);
}

static intnat hash_float32(value v)
{
  intnat h = 0;
  *(float *) &h = Float32_val(v);
  return h;
}

static struct custom_operations curses_float32_ops = {
  "fr.inria.caml.float32",
  custom_finalize_default,
  compare_float32,
  hash_float32,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Allocating an OCaml custom block to hold the given float * */
static value alloc_float(float w)
{
  value v = caml_alloc_custom(&curses_float32_ops, sizeof(float), 0, 1);
  Float32_val(v) = w;
  return v;
}

CAMLprim value caml_float32_equal(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_bool(Float32_val(v1) == Float32_val(v2)));
}

CAMLprim value caml_float32_compare(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(Val_int(compare_float32(v1, v2)));
}

CAMLprim value caml_float32_of_bytes(value s)
{
  CAMLparam1(s);
  CAMLreturn(alloc_float(*(float *) Bytes_val(s)));
}

CAMLprim value caml_float32_to_bytes(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(float));
  *(float *) Bytes_val(s) = Float32_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float32_of_string(value s)
{
  CAMLparam1(s);
  CAMLreturn (alloc_float(*(float *) String_val(s)));
}

CAMLprim value caml_float32_to_string(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  s = caml_alloc_string(sizeof(float));
  *(float *) String_val(s) = Float32_val(w);
  CAMLreturn(s);
}

CAMLprim value caml_float32_show(value w)
{
  CAMLparam1(w);
  CAMLlocal1(s);
  char *sp;
  int res = asprintf(&sp, "%f", Float32_val(w));
  if (res == -1) caml_failwith("caml_float32_show");
  s = caml_copy_string(sp);
  free(sp);
  CAMLreturn(s);
}

CAMLprim value caml_float32_read(value s)
{
  CAMLparam1(s);
  CAMLlocal1(v);
  float f;
  int res = sscanf(String_val(s), "%f", &f);
  if (res != 1) caml_failwith("caml_float32_read");
  v = alloc_float(f);
  CAMLreturn(v);
}

CAMLprim value caml_float32_neg(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(-Float32_val(v)));
}

CAMLprim value caml_float32_abs(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(fabsf(Float32_val(v))));
}

CAMLprim value caml_float32_sqrt(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(sqrtf(Float32_val(v))));
}

CAMLprim value caml_float32_ceil(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(ceilf(Float32_val(v))));
}

CAMLprim value caml_float32_floor(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(floorf(Float32_val(v))));
}

CAMLprim value caml_float32_round(value v)
{
  CAMLparam1(v);
  CAMLreturn(alloc_float(floorf(Float32_val(v))));
}

CAMLprim value caml_float32_is_nan(value v)
{
  CAMLparam1(v);
  CAMLreturn(Val_bool(isnan(Float32_val(v))));
}

CAMLprim value caml_float32_trunc(value v)
{
  CAMLparam1(v);
  CAMLreturn(caml_copy_int64((int64_t)(truncf(Float32_val(v)))));
}

CAMLprim value caml_float32_add(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_float(Float32_val(v1) + Float32_val(v2)));
}

CAMLprim value caml_float32_sub(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_float(Float32_val(v1) - Float32_val(v2)));
}

CAMLprim value caml_float32_mul(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_float(Float32_val(v1) * Float32_val(v2)));
}

CAMLprim value caml_float32_div(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLreturn(alloc_float(Float32_val(v1) / Float32_val(v2)));
}