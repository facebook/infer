#include <caml/mlvalues.h>

#ifdef _WIN32

#include <windows.h>

CAMLprim value number_of_cpus (value unit)
{
  SYSTEM_INFO sysinfo;
  GetSystemInfo (&sysinfo);
  int numcores = (int) sysinfo.dwNumberOfProcessors;
  return Val_int(numcores);
}

#elif defined (__linux__) || defined (__sun__) || defined (_AIX) \
  || defined (__APPLE__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
  || defined (__DragonFly__) || defined (__NetBSD__)

#include <unistd.h>

CAMLprim value number_of_cpus (value unit)
{
  int numcores = (int) sysconf (_SC_NPROCESSORS_ONLN);
  return Val_int(numcores);
}

#else

/* Backup. Extremely unlikely given the platforms supported by Infer */

CAMLprim value number_of_cpus (value unit)
{
  return Val_int(1);
}

#endif
