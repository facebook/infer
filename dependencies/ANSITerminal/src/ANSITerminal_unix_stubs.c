/* File: ANSITerminal_unix_stubs.c

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <sys/ioctl.h>
#include <termios.h>

/* Based on http://www.ohse.de/uwe/software/resize.c.html */
/* Inquire actual terminal size (this it what the kernel thinks - not
 * was the user on the over end of the phone line has really). */
CAMLexport
value ANSITerminal_term_size(value vfd)
{
  CAMLparam1(vfd);
  CAMLlocal1(vsize);
  int fd = Int_val(vfd);
  int x, y;
  
#ifdef TIOCGSIZE
  struct ttysize win;
#elif defined(TIOCGWINSZ)
  struct winsize win;
#endif

#ifdef TIOCGSIZE
  if (ioctl(fd, TIOCGSIZE, &win))
    caml_failwith("ANSITerminal.size");
  x = win.ts_cols;
  y = win.ts_lines;
#elif defined TIOCGWINSZ
  if (ioctl(fd, TIOCGWINSZ, &win))
    caml_failwith("ANSITerminal.size");
  x = win.ws_col;
  y = win.ws_row;
#else
  {
    const char *s;
    s = getenv("LINES");
    if (s)
      y = strtol(s,NULL,10);
    else
      y = 25;
    s = getenv("COLUMNS");
    if (s)
      x = strtol(s,NULL,10);
    else
      x = 80;
  }
#endif
    
  vsize = caml_alloc_tuple(2);
  Store_field(vsize, 0, Val_int(x));
  Store_field(vsize, 1, Val_int(y));
  CAMLreturn(vsize);
}

