/*
 * Copyright (c) Bloomberg L.P.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// works - OK
void empty_function_terminate() {
  return;
}

// works - OK
void one_liner_terminate(int x) {
  x++;
}


// works - OK
void two_liner_terminate(int x) {
  x++;
  return;
}


/* Pulse-inf: false negative: no support for infinite goto loops */
void simple_goto_not_terminate(int y) {
 re:
  y++;
  goto re;
}

/* pulse-inf: works -- empty path condition, no bug */
void simple_loop_terminate() {
  int y = 0;
  while (y < 100)
    y++;
}


// pulse-inf ok no loop
void simple_goto_terminate(int y) {
  y++;
  goto end;
 end:
  return;
}

/* pulse-inf: Able to flag bug */
void conditional_goto_not_terminate(int y) {
 re:
  if (y == 100)
    goto re;
  else
    return;
}


/* pulse-inf: works good */
extern void fcall(int y);

void loop_call_not_terminate(int y) {
  while (y == 100)
    fcall(y);
  return;
}



/* pulse-inf: FALSE NEGATIVE (no goto support) */
void twovars_goto_not_terminate(int y) {
  int z = y;
  int x = 0;
 label:
  x = 42;
  goto label;
}


/* pulse-inf: works good */
void loop_pointer_terminate(int *x, int y) {
  int *z = x;
  //int y = 1;
  if (x != &y)
    while (y < 100) {
      y++;
      (*z)--;
    }
}


/* pulse-inf: works good */
void loop_pointer_non_terminate(int *x, int y) {
  int *z = x;
  //int y = 1;
  if (x == &y)
    while (y < 100) {
      y++;
      (*z)--;
    }
}



/* pulse-inf: works good */
void var_goto_terminates(int y) {
  int x = 42;
  goto end;
  x++;
 end:
  return;
}


/* pulse-inf: works good */
void loop_conditional_not_terminate(int y) {
  int x = 0;
  //y = 0;
  while (y < 100)
    if (y < 50)
      x++;
    else
      y++;
}


/* pulse-inf: works good */
void loop_alternating_not_terminate(int y, int x) {
  int turn = 0;
  while (x < 100) {
    if (turn)
      x++;
    else 
      x--;
    turn = (turn ? 0 : 1);
  }
}



/* pulse-inf: works good */
void nested_loop_not_terminate(int y) {
  int x = 42;
  while (y < 100) {
    while (x <= 100) {
      if (x == 100)
	{
	  x = 1;
	  y = 0;
	}
      else
	x++;
    }
  }
}



/* pulse-inf: works good! */
void inner_loop_non_terminate(int y, int x) {
  while (y < 100)
    {
      while (x == 0)
	y++;
      y++;
    }
}


/* pulse-inf: works good */
void simple_dowhile_terminate(int y, int x) {
  do {
    y++;
    x++;
  } while (0);
}


/* pulse-inf: works good */
int conditional_goto_terminate(int x, int y) {
 re:
  x++;
  if (false)
    {
      int z1 = x * 2;
      goto re;
      return (z1);
    }
  else
    {
      int z2 = x + y;
      return z2;
    }
}




/* pulse-inf: works good */
void nested_loop_cond_not_terminate(int y) {
  int x = 42;
  while (y < 100) {
    while (x <= 100) {
      if (x == 100)
	{
	  x = 1;
	  y = y * 2;
	}
      else
	x++;
    }
    y++;
  }
}


/* pulse inf works */
void simple_loop_not_terminate(int y, int x) {
  //int x = 1;
  while (x != 3)
    y++;
}


/* pulse-inf: FP due to Pulse computing arithmetic on Q rather than BV */
void loop_signedarith_terminate(int y) {
  while (y > 0x7fffffff) {
    y++;
    y--;
  }
  return;
}


/* pulse-inf: FP due to Pulse computing arithmetic on Q rather than BV */
void goto_signedarith_terminate(int y) {
 re:
  if (y > 0x7fffffffffffffff)
    goto re;
  else
    return;
}


/* pulse-inf: works good! no bug */
void loop_with_break_terminate(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      break;
    else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_break_terminate_var1(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      {
	y--;
	break;
      }
    else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_break_terminate_var2(int y) {
  while (y < 100)
    if (y == 50)
      {
	y--;
	break;
      }
    else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_break_terminate_var3(int y) {
  while (y < 100)
    if (y == 50)
      break;
    else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_return_terminate(int y) {
  while (y < 100)
    if (y == 50)
      {
	y--;
	return;
      }
    else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_return_terminate_var1(int y) {
  while (y < 100)
    if (y == 50)
      return;
    else
      y++;
}


/* pulse-inf: works good! no bug */
void loop_with_return_terminate_var2(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      {
	y--;
	return;
      }
    else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_return_terminate_var3(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      return;
    else
      y++;
}

/* pulse-inf: False negative -- maybe augment the numiters in pulseinf config */
// From: Gupta POPL 2008 - "Proving non-termination"
int bsearch_non_terminate_gupta08(int a[], int k,
				  unsigned int lo,
				  unsigned int hi) {
  unsigned int mid;
  
  while (lo < hi) {
    mid = (lo + hi) / 2;
    if (a[mid] < k) {
      lo = mid + 1;
    } else if (a[mid] > k) {
      hi = mid - 1;
    } else {
      return mid;
    }
  }
  return -1;
}


/* pulseinf: works fine - no bug detected */
// Cook et al. 2006 - TERMINATOR fails to prove termination
typedef struct  s_list{
  int		value;
  struct s_list *next;
}		list_t;

/* pulse-inf: works good, no bug */
static void
list_iter_terminate_cook06(list_t *p) {
  int tot = 0;
  do {
    tot += p->value;
    p = p->next;
  }
  while (p != 0);
}


/* pulse-inf: works good, no bug */
static void
list_iter_terminate_cook06_variant(list_t *p) {
  int tot = 0;
  while (p != 0) {
    tot += p->value;
    p = p->next;
  }
}

/* pulse-inf: works good - no bug */
static void
list_iter_terminate_cook06_variant2(list_t *p) {
  int tot;
  for (tot = 0; p != 0; p = p->next) {
    tot += p->value;
  }
} 

/* pulse-inf: works good - no bug */
// Cook et al. 2006 - TERMINATOR proves termination
void two_ints_loop_terminate_cook06(int x, int y)
{
  if (y >= 1) 
    while (x >= 0)
      x = x + y;
}



/* Cook et al. 2006 - Prove termination with non-determinism involved */
int Ack(int x, int y)
{
  if (x>0) {
    int n;
    if (y>0) {
      y--;
      n = Ack(x,y);
    } else {
      n = 1;
    }
    x--;
    return Ack(x,n);
  } else {
    return y+1;
  }
}

#include <stdlib.h>

/* pulse-inf: works good! no bug */
int nondet() { return (rand()); }
int benchmark_terminate_nondet_cook06()
{
  int x = nondet();
  int y = nondet();

  int * p = &y;
  int * q = &x;
  bool b = true;
  
  while (x<100 && 100<y && b)
    {
      if (p==q) {
	int k = Ack(nondet(),nondet());
	(*p)++;
	while((k--)>100)
	  {
	    if (nondet()) {p = &y;}
	    if (nondet()) {p = &x;}
	    if (!b) {k++;}
	  }
      } else {
	(*q)--;
	(*p)--;
	if (nondet()) {p = &y;}
	if (nondet()) {p = &x;}
      }
      b = nondet();
    }
  return (0);
}


/* pulseinf: works good - no bug detected */
// Cook et al. 2006 - termination with non determinism 
//#include <stdlib.h>
//int nondet() { return (rand()); }
int npc = 0;
int nx, ny, nz;
void benchmark_terminate_cook06()
{
  int x = nondet(), y = nondet(), z = nondet();
  if (y>0) {
    do {
      if (npc == 5) {
	if (!( (y < z && z <= nz)
	       || (x < y && x >= nx)
	       || 0))
	  ;
      }
      if (npc == 0) {
	if (nondet()) {
	  nx = x;
	  ny = y;
	  nz = z;
	  npc = 5;
	}
      }
      if (nondet()) {
	x = x + y;
      } else {
	z = x - y;
      }
    } while (x < y && y < z);
  }
}


/* pulseinf: works good - no bug */
// Cook et al. 2006 proves termination with non determinism 
//#include <stdlib.h>
//int	nondet() { return (rand()); }
void	benchmark_simple_terminate_cook06()
{
  int x = nondet(), y = nondet(), z=nondet();
  if (y > 0) {
    do {
      if (nondet()) {
	x = x + y;
      }
      else
	{
	  z = x - y;
	}
    } while (x < y && y < z);
  }
}



/* Simple non-det benchmark for non-terminate */
/* Inspired by cook'06 by flipping existing test benchmark_simple_terminate_cook06 */
/* pulse-inf: works good! flag the bug */
//#include <stdlib.h>
//int	nondet() { return (rand()); }
void	nondet_loop_non_terminate(int z)
{
  int x = 1;
  while (x < z)
    if (nondet())
      x++;
}


/* From: AProVE: Non-termination proving for C Programs (Hensel et al. TACAS 2022)*/
/* pulse-inf: Works good! (flag bug) */
void hensel_tacas22_non_terminate(int x, int y)
{
  y = 0;
  while (x > 0)
    {
      x--;
      y++;
    }
  while (y > 1)
    y = y;
}


/* Harris et al. "Alternation for Termination (SAS 2010) - Terminating program */
//#include <stdlib.h>
//int	nondet() { return (rand()); }
void	foo(int *x) {
  (*x)--;
}

/* Pulse-inf: FP */
void interproc_terminating_harris10(int x) {
  while (x > 0)
    foo(&x);
}

/* Derived from Harris'10 - Pulse-inf: FP! */
void interproc_terminating_harris10_cond(int x) {
  while (x > 0)
    {
      if (nondet()) foo(&x);
      else foo(&x);
    }
}


/* Harris et al. "Alternation for Termination (SAS 2010) - Non Terminating program */
/* TERMINATOR unable to find bug */
/* TREX find bug in 5sec */
/* pulse-inf: works good! Detect the bug! */
void loop_non_terminating_harris10(int x, int d, int z)
{
  d = 0;
  z = 0;
  while (x > 0) {
    z++;
    x = x - d;
  }
}


/* Berdine et al. "Automatic termination proofs for programs with shape-shifting heaps" (CAV'06) */
/* Need to find the termination bug */
/* Does not compile */
/*
void non_terminate_berdine06() {
  for (entry = DeviceExtension->ReadQueue.Flink;
       entry != &DeviceExtension->ReadQueue;
       entry = entry->Flink) {
    irp = (IRP *)((CHAR *)(entry)-(ULONG *)(&((IRP *)0)->Tail.Overlay.ListEntry));
    stack = IoGetCurrentIrpStackLocation (irp);
    if (stack->FileObject == FileObject) {
      RemoveEntryList(entry);
      if (IoSetCancelRoutine (irp, NULL)) {
	return irp;
      } else {
	InitializeListHead (&irp->Tail.Overlay.ListEntry);
      }
    }
  }
}
*/



  
/*** Chen et al. TACAS 2014 */
// TNT proves non-termination with non determinism
/* Pulse-inf: works good (also flag the bug)
/* TO me: there is no bug here! problem in chen14 paper - the nondet() should eventually make it break */
//#include <stdlib.h>
//int	nondet() { return (rand()); }
void nondet_nonterminate_chen14(int k, int i) {
  if (k >= 0)
    ;
  else
    i = -1;
  while (i >= 0)
    i = nondet();
  i = 2;
}


/* pulse-inf: works good! finds bug */
// TNT proves non-termination
void nestedloop_nonterminate_chen14(int i) {
  if (i == 10) {
    while (i > 0) {
      i = i - 1;
      while (i == 0)
	;
    }
  }
}



// TNT fails to prove non-termination
/* pulse-inf says there is no bug */
/* To me: this will terminate because k >= 0 will eventually be false due to integer wrap */
void nestedloop2_nonterminate_chen14(int k, int j) {
  while (k >= 0) {
    k++;
    j = k;
    while (j >= 1)
      j--;
  }
}



/*
Utility function with a termination bug
From "Termination Proofs for Systems Code" by Cook et al. (PLDI 2006)
Does not compile
*/
/*
NTSTATUS
Serenum_ReadSerialPort_cook06(CHAR * PReadBuffer, USHORT Buflen,
			      ULONG Timeout, USHORT * nActual,
			      IO_STATUS_BLOCK * PIoStatusBlock,
			      const FDO_DEVICE_DATA * FdoData)
{
  NTSTATUS status;
  IRP * pIrp;
  LARGE_INTEGER startingOffset;
  KEVENT event;
  SERIAL_TIMEOUTS timeouts;
  ULONG i;
  
  startingOffset.QuadPart = (LONGLONG) 0;
  //
  // Set the proper timeouts for the read
  //
  
  timeouts.ReadIntervalTimeout = MAXULONG;
  timeouts.ReadTotalTimeoutMultiplier = MAXULONG;
  timeouts.ReadTotalTimeoutConstant = Timeout;
  timeouts.WriteTotalTimeoutMultiplier = 0;
  timeouts.WriteTotalTimeoutConstant = 0;
  KeInitializeEvent(&event, NotificationEvent, FALSE);
  status = Serenum_IoSyncIoctlEx(IOCTL_SERIAL_SET_TIMEOUTS, FALSE,FdoData->TopOfStack,
				 &event, &timeouts, sizeof(timeouts), NULL, 0);
  if (!NT_SUCCESS(status)) {
    return status;
  }
  
  Serenum_KdPrint(FdoData, SER_DBG_SS_TRACE, ("Read pending...\n"));
  
  *nActual = 0;
  
  while (*nActual < Buflen) {
    KeClearEvent(&event);
    pIrp = IoBuildSynchronousFsdRequest(IRP_MJ_READ, FdoData->TopOfStack,
					PReadBuffer, 1, &startingOffset,
					&event, PIoStatusBlock);
    if (pIrp == NULL) {
      Serenum_KdPrint(FdoData, SER_DBG_SS_ERROR, ("Failed to allocate IRP\n"));
      return STATUS_INSUFFICIENT_RESOURCES;
    }
    status = IoCallDriver(FdoData->TopOfStack, pIrp);
    
    if (status == STATUS_PENDING) {
      status = KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
      if (status == STATUS_SUCCESS) {
	status = PIoStatusBlock->Status;
      }
    }
    
    if (!NT_SUCCESS(status) || status == STATUS_TIMEOUT) {
      Serenum_KdPrint (FdoData, SER_DBG_SS_ERROR, ("IO Call failed with status %x\n", status));
      return status;
    }
    *nActual += (USHORT)PIoStatusBlock->Information;
    PReadBuffer += (USHORT)PIoStatusBlock->Information;
  }
  return status;
}
*/


/*** Gupta et al. POPL 2008 -- Does not compile
static void
_mmpt_insert_gupta08(struct mmpt* mmpt, unsigned long base, unsigned long* len, int prot,
                     tab_t* tab, int level, int* nonzero, int allocate_ok) {
                     unsigned int idx; tab_t entry;
if(*len == 0) return;
  idx = make_idx(mmpt, base, level);
  if(level < 2 && base == tab_base(mmpt, base, level + 1) && *len >= tab_len(mmpt, level + 1)) {
    // CASE A: Upper level, new region is aligned & spans at least one entry
    unsigned int entry_len;
    if(tab[idx] && !uentry_is_data(mmpt, tab[idx])) {
      look_for_nonzero(mmpt, (tab_t*)tab[idx], level, nonzero);
      table_free(mmpt, (void*)tab[idx], level + 1);
      tab[idx] = 0;
    }
    entry = tab[idx];
    entry_len = make_entry(mmpt, base, *len, prot, level, &entry);
    tab[idx] = entry;
    *len -= entry_len;
    base += entry_len;
    _mmpt_insert(mmpt, base, len, prot, mmpt->tab, 0, nonzero, allocate_ok);
  } else if(level < 2 && tab[idx] && !uentry_is_data(mmpt, tab[idx])) {
    // CASE B: Upper level, pointer entry
    // Recurse down through pointer
    _mmpt_insert(mmpt, base, len, prot, (tab_t*)tab[idx], level + 1, nonzero, allocate_ok);
  } else if(level < 2 && ((base & (subblock_len(mmpt, level)-1)) != 0 || *len < subblock_len(mmpt, level))) {
    // CASE C: Upper level, NULL or data entry, new region doesn’t fit in
    // subblock (not aligned or not big enough)
    unsigned long upper_data_entry = tab[idx];
    unsigned int i;
    *nonzero |= (tab[idx] != 0);
    if(allocate_ok) {
      unsigned long sub_len;
      tab[idx] = (tab_t)xmalloc(tab_nentries(mmpt, level + 1) * sizeof(*mmpt->tab));
      memset((tab_t*)tab[idx], 0, tab_nentries(mmpt, level + 1) * sizeof(*mmpt->tab));
      for(i = 0; i < 1<<mmpt->lg_num_subblock[level]; ++i) {
	sub_len = subblock_len(mmpt, level);
	_mmpt_insert(mmpt, tab_base(mmpt, base, level+1) + i * subblock_len(mmpt, level), &sub_len,
		     entry_prot(mmpt, upper_data_entry, i), (tab_t*)tab[idx], level + 1, nonzero, allocate_ok);
      }
      _mmpt_insert(mmpt, base, len, prot, (tab_t*)tab[idx], level + 1, nonzero, allocate_ok);
    } else {
      unsigned int tlen = tab_len(mmpt, level + 1);
      // CASE D: Upper level, NULL or data entry, new region doesn’t fit in
      // subblock (not aligned or not big enough), and not
      // allocating new tables
      if(*len < tlen) return;
      *len -= tlen;
      _mmpt_insert(mmpt, tab_addr(mmpt, base, idx+1, level), len, prot,
		   mmpt->tab, 0, nonzero, allocate_ok);
    }
  } else {
    // CASE E: Any level, NULL or data entry, fill in the rest of
    // this table and recurse for the remainder if necessary.
    for(; *len >= subblock_len(mmpt, level)
	  && idx < tab_nentries(mmpt, level); idx++) {
      int entry_len;
      *nonzero |= (tab[idx] != 0);
      entry = tab[idx];
      entry_len = make_entry(mmpt, base, *len, prot, level, &entry);
      tab[idx] = entry;
      *len -= entry_len;
      base += entry_len;
    }
    _mmpt_insert(mmpt, base, len, prot, mmpt->tab, 0, nonzero, allocate_ok);
  }
}



****/

