module C = Configurator.V1

let sched_setaffinity_code = {|
#define _GNU_SOURCE
#include <sched.h>
int main()
{
  cpu_set_t cpus;
  return sched_setaffinity(0, sizeof(cpu_set_t), &cpus);
}
|}

let thread_policy_set_code = {|
#include <mach/mach_init.h>
#include <mach/thread_policy.h>
int main()
{
  thread_affinity_policy_data_t affinityData;
  return thread_policy_set(mach_thread_self(), THREAD_AFFINITY_POLICY, &affinityData, THREAD_AFFINITY_POLICY_COUNT);
}
|}

let int_of_bool b = if b then 1 else 0

let () =
  C.main ~name:"parmap" (fun c ->
    let ml_file = "parmap_compat.ml" in
    let pre_406 = [ "let map_file = Bigarray.Genarray.map_file" ] in
    let ml_code = match C.ocaml_config_var_exn c "version" with
      | "4.03.0" -> pre_406
      | "4.04.0" -> pre_406
      | "4.04.1" -> pre_406
      | "4.04.2" -> pre_406
      | "4.05.0" -> pre_406
      | _ -> [ "let map_file = Unix.map_file" ]
    in
    C.Flags.write_lines ml_file ml_code ;

    let has_sched_setaffinity = C.c_test c sched_setaffinity_code in
    let has_thread_policy_set = C.c_test c thread_policy_set_code in
    C.C_define.gen_header_file c ~fname:"setcore_stubs.h"
      [ "HAVE_DECL_SCHED_SETAFFINITY", Int (int_of_bool has_sched_setaffinity) ;
       "HAVE_MACH_THREAD_POLICY_H", Int (int_of_bool has_thread_policy_set)  ] ;
  );
