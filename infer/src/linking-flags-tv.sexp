; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.

; HOWTO generate this file: see linking-flags.sexp

(-noautolink
 -cclib -Wl,-Bstatic
 -cclib -lsqlite3_stubs -cclib -lsqlite3
 -cclib -Wl,-Bdynamic
 -cclib -lcamlzip -cclib -lz -cclib -lpthread -cclib -lparmap_stubs -cclib -lmtime_clock_stubs
 -cclib -lrt -cclib -lcamlstr -cclib -lANSITerminal_stubs -cclib -lasync_rpc_stubs
 -cclib -lasync_unix_stubs -cclib -ltime_stamp_counter_stubs -cclib -locaml_intrinsics_stubs
 -cclib -llinux_ext_stubs -cclib -lcore_thread_stubs -cclib -liobuf_unix_stubs
 -cclib -lfilename_unix_stubs -cclib -lsys_unix_stubs -cclib -lbigstring_unix_stubs
 -cclib -locaml_c_utils_stubs -cclib -lcore_unix_stubs -cclib -lspawn_stubs
 -cclib -lsignal_unix_stubs -cclib -lerror_checking_mutex_stubs -cclib -lthreadsnat -cclib -lpthread
 -cclib -lcore_stubs -cclib -lbase_bigstring_stubs -cclib -lexpect_test_collector_stubs
 -cclib -ltime_now_stubs -cclib -lbin_prot_stubs -cclib -lunix -cclib -lbase_stubs
 -cclib -lbase_internalhash_types_stubs -cclib -lasmrun -cclib -lm -cclib -ldl
)
