(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module CLOpt = CommandLineOption
module L = Logging

let infer_profile_name = "infer-capture"

let infer_profile =
  lazy
    (* indented so that users may copy it into their projects if they want to *)
    (Printf.sprintf
       {|
    <profile>
      <id>%s</id>
      <build>
        <plugins>
          <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-compiler-plugin</artifactId>
            <version>3.11.0</version>
            <configuration>
              <compilerId>javac</compilerId>
              <forceJavacCompilerUse>true</forceJavacCompilerUse>
              <fork>true</fork>
              <executable>%s</executable>
              %s
            </configuration>
          </plugin>
        </plugins>
      </build>
    </profile>|}
       infer_profile_name
       (Config.bin_dir ^/ InferCommand.infer_exe_name)
       ( match Config.java_version with
       | Some version when version >= 9 ->
           Printf.sprintf "<release>%d</release>" version
       | _ ->
           "" ) )


let pom_worklist = ref [CLOpt.init_work_dir]

let add_infer_profile_to_xml dir maven_xml infer_xml =
  let copy xml_in xml_out = Xmlm.output xml_out (Xmlm.input xml_in) in
  (* whether we ever found a <profiles> tag *)
  let found_profiles_tag = ref false in
  (* whether there already is an infer profile --this will always be true at the end *)
  let found_infer_profile = ref false in
  (* Process an xml document from the root. Assume the Dtd has already been handled. *)
  let rec process_root xml_in xml_out = process xml_in xml_out []
  and insert_infer_profile xml_out =
    let infer_xml = Xmlm.make_input ~strip:false (`String (0, Lazy.force infer_profile)) in
    Xmlm.input infer_xml |> ignore ;
    (* skip dummy DTD *)
    process_root infer_xml xml_out
  and process xml_in xml_out tag_stack =
    let elt_in = Xmlm.input xml_in in
    match elt_in with
    | `El_start tag ->
        Xmlm.output xml_out elt_in ;
        let tag_name = snd (fst tag) in
        if String.equal tag_name "profiles" then found_profiles_tag := true ;
        process xml_in xml_out (tag_name :: tag_stack)
    | `El_end -> (
        ( match tag_stack with
        | "profiles" :: _ when not !found_infer_profile ->
            (* found the </profiles> tag but no infer profile found, add one *)
            insert_infer_profile xml_out
        | [_] when not !found_profiles_tag ->
            (* closing the root tag but no <profiles> tag found, add
               <profiles>[infer profile]</profiles> *)
            Xmlm.output xml_out (`El_start (("", "profiles"), [])) ;
            found_profiles_tag := true ;
            (* do not add <profiles> again *)
            insert_infer_profile xml_out ;
            Xmlm.output xml_out `El_end
        | _ ->
            () ) ;
        Xmlm.output xml_out elt_in ;
        match tag_stack with
        | _ :: parent :: tl ->
            process xml_in xml_out (parent :: tl)
        | [_] ->
            (* closing the first tag, we're done *)
            ()
        | [] ->
            L.(die UserError) "ill-formed xml" )
    | `Data data ->
        Xmlm.output xml_out elt_in ;
        ( match tag_stack with
        | "id" :: "profile" :: "profiles" :: _ when String.equal data infer_profile_name ->
            L.(debug Capture Quiet) "Found infer profile, not adding one@." ;
            found_infer_profile := true
        | "module" :: "modules" :: _ ->
            let abs_data = dir ^/ data in
            L.(debug Capture Quiet) "Adding maven module %s@." abs_data ;
            pom_worklist := abs_data :: !pom_worklist
        | _ ->
            () ) ;
        process xml_in xml_out tag_stack
    | `Dtd _ ->
        (* already processed the Dtd node *)
        assert false
  in
  let process_document () =
    (* process `Dtd; if present, it is always the first node *)
    ( match Xmlm.peek maven_xml with
    | `Dtd _ ->
        copy maven_xml infer_xml
    | _ ->
        Xmlm.output infer_xml (`Dtd None) ) ;
    process_root maven_xml infer_xml ;
    Xmlm.eoi maven_xml |> ignore ;
    if not (Xmlm.eoi maven_xml) then L.(die UserError) "More than one document"
  in
  process_document ()


let add_infer_profile mvn_pom infer_pom =
  let ic = In_channel.create mvn_pom in
  let with_oc out_chan =
    let with_ic () =
      let xml_in =
        Xmlm.make_input ~strip:false (`Channel ic) ~ns:(fun ns -> Some ns)
        (* be generous with namespaces *)
      in
      let xml_out =
        Xmlm.make_output ~nl:true (`Channel out_chan) ~ns_prefix:(fun prefix -> Some prefix)
        (* be generous with namespaces *)
      in
      add_infer_profile_to_xml (Filename.dirname mvn_pom) xml_in xml_out
    in
    protect ~f:with_ic ~finally:(fun () -> In_channel.close ic)
  in
  try Utils.with_file_out infer_pom ~f:with_oc
  with Xmlm.Error ((line, col), error) ->
    L.die ExternalError "%s:%d:%d: ERROR: %s" mvn_pom line col (Xmlm.error_message error)


let add_profile_to_pom_in_directory dir =
  (* Even though there is a "-f" command-line arguments to change the config file Maven reads from,
     this is unreliable and Maven pretty much always reads from "pom.xml" anyway. So, we replace
     "pom.xml" with a version holding a special profile for infer capture, then put the original
     back in place. *)
  let maven_pom_path = dir ^/ "pom.xml" in
  let saved_pom_path = dir ^/ "pom.xml.infer-orig" in
  let infer_pom_path = dir ^/ "pom.xml.infer" in
  add_infer_profile maven_pom_path infer_pom_path ;
  Unix.rename ~src:maven_pom_path ~dst:saved_pom_path ;
  Epilogues.register
    ~f:(fun () -> Unix.rename ~src:saved_pom_path ~dst:maven_pom_path)
    ~description:"restoring Maven's pom.xml to its original state" ;
  Unix.rename ~src:infer_pom_path ~dst:maven_pom_path ;
  if Config.debug_mode then
    Epilogues.register
      ~f:(fun () -> Unix.rename ~src:maven_pom_path ~dst:infer_pom_path)
      ~description:"saving infer's pom.xml"


let capture ~prog ~args =
  while not (List.is_empty !pom_worklist) do
    let pom = List.hd_exn !pom_worklist in
    pom_worklist := List.tl_exn !pom_worklist ;
    add_profile_to_pom_in_directory pom
  done ;
  let extra_args = ["-P"; infer_profile_name] in
  let capture_args = args @ extra_args in
  L.(debug Capture Quiet)
    "Running maven capture:@\n%s %s@." prog
    (String.concat ~sep:" " (List.map ~f:(Printf.sprintf "'%s'") capture_args)) ;
  (* let children infer processes know that they are spawned by Maven *)
  Unix.fork_exec ~prog ~argv:(prog :: capture_args) ~env:Config.env_inside_maven ()
  |> Unix.waitpid
  |> function
  | Ok () ->
      ()
  | Error _ as status ->
      L.(die UserError)
        "*** Maven command failed:@\n*** %s@\n*** %s@\n"
        (String.concat ~sep:" " (prog :: capture_args))
        (Unix.Exit_or_signal.to_string_hum status)
