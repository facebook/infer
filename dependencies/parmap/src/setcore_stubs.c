#include "setcore_stubs.h"
#if HAVE_MACH_THREAD_POLICY_H
#include <mach/mach_init.h>
#include <mach/thread_policy.h>
// #include <mach/sched.h>
#endif
#if HAVE_DECL_SCHED_SETAFFINITY
#define _GNU_SOURCE
#include <sched.h>
#endif
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <caml/mlvalues.h>
#ifdef _WIN32
#include <windows.h>
#endif

static int get_numcores() {
#ifdef _WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return sysinfo.dwNumberOfProcessors;
#else
  return sysconf( _SC_NPROCESSORS_ONLN );
#endif
}
CAMLprim value numcores(value unit) {
  int numcores = get_numcores();
  return Val_int(numcores);
}

CAMLprim value setcore(value which) {
  int numcores = get_numcores();
  int w = Int_val(which) % numcores; // stay in the space of existing cores
#if HAVE_DECL_SCHED_SETAFFINITY
  cpu_set_t cpus;   
#endif
#if HAVE_MACH_THREAD_POLICY_H
  thread_affinity_policy_data_t affinityData;
#endif
  int retcode;
  int finished=0;
  if (numcores <= 1) // only one core in the system, no need to attempt pinning
    return Val_unit;
  while (finished==0)
    {
#if HAVE_DECL_SCHED_SETAFFINITY
      CPU_ZERO(&cpus); 
      CPU_SET (w,&cpus);
      //fprintf(stderr,"Trying to pin to cpu %d out of %d reported by the system\n",w,numcores);
      retcode = sched_setaffinity(getpid(), sizeof(cpu_set_t), &cpus);
      if(retcode != 0) {
	fprintf(stderr,"Failed pinning to cpu %d, trying %d/2\n",w, w); 
	w=w/2;
      }
      else
#endif
#if HAVE_MACH_THREAD_POLICY_H
      affinityData.affinity_tag = w;
      retcode = thread_policy_set(mach_thread_self(),
                        THREAD_AFFINITY_POLICY,
                        &affinityData,
                        THREAD_AFFINITY_POLICY_COUNT);
      if(retcode) {
        fprintf(stderr,"MAC OS X: Failed pinning to cpu %d, trying %d/2\n",w, w);
        w=w/2;
      }
      else 
#endif
	{ //fprintf(stderr,"Succeeded pinning to cpu %d\n",w); 
	  finished=1;
	}
    }
  return Val_unit;
}
