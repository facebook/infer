/***********************************************************************/
/*                                                                     */
/*                      The CamlZip library                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Lesser General Public License, with     */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Stub code to interface with Zlib */

#include <stdint.h>
#include <zlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/custom.h>

#define ZStream_val(v) (*((z_streamp *) Data_custom_val(v)))

static const value * camlzip_error_exn = NULL;

static void camlzip_error(char * fn, value vzs)
{
  char * msg;
  value s1 = Val_unit, s2 = Val_unit, bucket = Val_unit;

  msg = ZStream_val(vzs)->msg;
  if (msg == NULL) msg = "";
  if (camlzip_error_exn == NULL) {
    camlzip_error_exn = caml_named_value("Zlib.Error");
    if (camlzip_error_exn == NULL)
      caml_invalid_argument("Exception Zlib.Error not initialized");
  }
  Begin_roots3(s1, s2, bucket);
    s1 = caml_copy_string(fn);
    s2 = caml_copy_string(msg);
    bucket = caml_alloc_small(3, 0);
    Field(bucket, 0) = *camlzip_error_exn;
    Field(bucket, 1) = s1;
    Field(bucket, 2) = s2;
  End_roots();
  caml_raise(bucket);
}

static void camlzip_free_dstream(value vzs)
{
  deflateEnd(ZStream_val(vzs));
  caml_stat_free(ZStream_val(vzs));
  ZStream_val(vzs) = NULL;
}

static struct custom_operations camlzip_dstream_ops = {
  "camlzip_dstream_ops", &camlzip_free_dstream, NULL, NULL, NULL, NULL
};

value camlzip_deflateInit(value vlevel, value expect_header)
{
  value vzs =
    caml_alloc_custom_mem(&camlzip_dstream_ops,
                          sizeof(z_streamp), sizeof(z_stream));
  ZStream_val(vzs) = caml_stat_alloc(sizeof(z_stream));
  /* Zlib API: the fields zalloc, zfree and opaque must be initialized */
  ZStream_val(vzs)->zalloc = NULL;
  ZStream_val(vzs)->zfree = NULL;
  ZStream_val(vzs)->opaque = NULL;
  if (deflateInit2(ZStream_val(vzs),
                   Int_val(vlevel),
                   Z_DEFLATED,
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS,
                   8,
                   Z_DEFAULT_STRATEGY) != Z_OK)
    camlzip_error("Zlib.deflateInit", vzs);
  return vzs;
}

static int camlzip_flush_table[] = 
{ Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH };

value camlzip_deflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = deflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if (retcode < 0 && retcode != Z_BUF_ERROR) camlzip_error("Zlib.deflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;         /* not required, but cleaner */
  zs->next_out = NULL;        /* (avoid dangling pointers into Caml heap) */
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_deflate_bytecode(value * arg, int nargs)
{
  return camlzip_deflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_deflateEnd(value vzs)
{
  if (deflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.deflateEnd", vzs);
  return Val_unit;
}

static void camlzip_free_istream(value vzs)
{
  inflateEnd(ZStream_val(vzs));
  caml_stat_free(ZStream_val(vzs));
  ZStream_val(vzs) = NULL;
}

static struct custom_operations camlzip_istream_ops = {
  "camlzip_dstream_ops", &camlzip_free_istream, NULL, NULL, NULL, NULL
};

value camlzip_inflateInit(value expect_header)
{
  value vzs =
    caml_alloc_custom_mem(&camlzip_istream_ops,
                          sizeof(z_streamp), sizeof(z_stream));
  /* Zlib API: The fields next_in, avail_in, zalloc, zfree and opaque
     must be initialized */
  ZStream_val(vzs) = caml_stat_alloc(sizeof(z_stream));
  ZStream_val(vzs)->zalloc = NULL;
  ZStream_val(vzs)->zfree = NULL;
  ZStream_val(vzs)->opaque = NULL;
  ZStream_val(vzs)->next_in = NULL;
  ZStream_val(vzs)->avail_in = 0;
  if (inflateInit2(ZStream_val(vzs),
                   Bool_val(expect_header) ? MAX_WBITS : -MAX_WBITS) != Z_OK)
    camlzip_error("Zlib.inflateInit", vzs);
  return vzs;
}

value camlzip_inflate(value vzs, value srcbuf, value srcpos, value srclen,
                      value dstbuf, value dstpos, value dstlen,
                      value vflush)
{
  z_stream * zs = ZStream_val(vzs);
  int retcode;
  long used_in, used_out;
  value res;

  zs->next_in = &Byte_u(srcbuf, Long_val(srcpos));
  zs->avail_in = Long_val(srclen);
  zs->next_out = &Byte_u(dstbuf, Long_val(dstpos));
  zs->avail_out = Long_val(dstlen);
  retcode = inflate(zs, camlzip_flush_table[Int_val(vflush)]);
  if ((retcode < 0 && retcode != Z_BUF_ERROR) || retcode == Z_NEED_DICT)
    camlzip_error("Zlib.inflate", vzs);
  used_in = Long_val(srclen) - zs->avail_in;
  used_out = Long_val(dstlen) - zs->avail_out;
  zs->next_in = NULL;           /* not required, but cleaner */
  zs->next_out = NULL;          /* (avoid dangling pointers into Caml heap) */
  res = caml_alloc_small(3, 0);
  Field(res, 0) = Val_bool(retcode == Z_STREAM_END);
  Field(res, 1) = Val_int(used_in);
  Field(res, 2) = Val_int(used_out);
  return res;
}

value camlzip_inflate_bytecode(value * arg, int nargs)
{
  return camlzip_inflate(arg[0], arg[1], arg[2], arg[3],
                         arg[4], arg[5], arg[6], arg[7]);
}

value camlzip_inflateEnd(value vzs)
{
  if (inflateEnd(ZStream_val(vzs)) != Z_OK)
    camlzip_error("Zlib.inflateEnd", vzs);
  return Val_unit;
}

value camlzip_update_crc32(value crc, value buf, value pos, value len)
{
  return caml_copy_int32(crc32((uint32_t) Int32_val(crc), 
                          &Byte_u(buf, Long_val(pos)),
                          Long_val(len)));
}

