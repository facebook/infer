; Copyright (c) Facebook, Inc. and its affiliates.
;
; This source code is licensed under the MIT license found in the
; LICENSE file in the root directory of this source tree.

; ## HOWTO generate this file
;
; 1. Build infer without PLATFORM_ENV set and in VERBOSE mode, e.g. `make -C infer/src VERBOSE=1`
; 2. Check the dune logs in infer/_build/log. At the very end is the linking command, which
;    contains linking directives starting with `-l`, e.g.
;
; '-lcamlstrnat' '-lcomprmarsh' '-lzarith' '-lgmp' '-lcamlzip' '-lz' '-lsqlite3_stubs' '-lsqlite3' '-lparmap_stubs' '-lmtime_clock_stubs' '-lrt' '-lfilename_unix_stubs' '-lsys_unix_stubs' '-lcore_unix_stubs' '-lspawn_stubs' '-lsignal_unix_stubs' '-lerror_checking_mutex_stubs' '-lthreadsnat' '-lANSITerminal_stubs' '-lpyml_stubs' '-lunixnat' '-lcore_stubs' '-lheap_block_stubs' '-lbase_bigstring_stubs' '-lexpect_test_collector_stubs' '-ltime_now_stubs' '-lbin_prot_stubs' '-lbase_stubs' '-lbase_internalhash_types_stubs'
;
;  3. Copy those below as follows: Keep the flags the same until the dynamic section after `-cclib
;     -Wl,-Bdynamic`. The flags already included are for static libraries; we want to keep these the
;     same. Update the dynamic flags by copying all the ones from the log and prefixing each of them
;     with `-cclib`. Beware not to include the libraries already linked statically in the list of
;     dynamically-linked ones either. Put the -lLLVM* flags, without the flags from the static
;     section, in linking-flags-llvm.sexp, as well as what is llvm support specific.
;  4. Update the example above to match the new flags so we can refer to it to infer the
;     correspondence log link flags -> this file.
;  5. Tips and tricks to come up with the magic flag list that will work:
;     - do not include -lasmrun, this is for native builds only and will mess up the bytecode builds
;     - you can keep only one flag of each library; if the same flag appears several times, keep the
;       last occurence of it. (The linker expects each library to be added before its own
;       dependencies.)
;     - add -verbose to the list below to print the linker command, which you can then rerun
;       yourself
;
; ## Why this file
;
; We want to statically link some particular libraries into our infer binaries to distribute them
; internally where they might not be available. Other libraries are fine to remain dynamically
; linked.

(
 -cclib -Wl,-Bstatic
 -cclib -lzarith -cclib -lgmp -cclib -lsqlite3_stubs -cclib -lsqlite3
 -cclib -Wl,-Bdynamic
 -cclib -lcomprmarsh -cclib -lcamlzip -cclib -lz -cclib -lparmap_stubs
 -cclib -lmtime_clock_stubs -cclib -lrt -cclib -lcamlstrnat -cclib -lspawn_stubs -cclib -lthreadsnat -cclib -lpthread
 -cclib -lANSITerminal_stubs -cclib -lpyml_stubs -cclib -lcore_stubs -cclib -lheap_block_stubs
 -cclib -lbase_bigstring_stubs -cclib -lexpect_test_collector_stubs -cclib -ltime_now_stubs
 -cclib -lbin_prot_stubs -cclib -lunixnat -cclib -lbase_stubs -cclib -lbase_internalhash_types_stubs
 -cclib -lm -cclib -ldl
)
