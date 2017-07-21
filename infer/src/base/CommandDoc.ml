(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module CLOpt = CommandLineOption

type data = {name: string; command_doc: CLOpt.command_doc}

let inferconfig_env_var = "INFERCONFIG"

let infer_exe_name = "infer"

(** Name of the infer configuration file *)
let inferconfig_file = ".inferconfig"

let command_to_name =
  let open CLOpt in
  [ (Analyze, "analyze")
  ; (Capture, "capture")
  ; (Compile, "compile")
  ; (Diff, "diff")
  ; (Report, "report")
  ; (ReportDiff, "reportdiff")
  ; (Run, "run") ]

let name_of_command = List.Assoc.find_exn ~equal:CLOpt.equal_command command_to_name

let exe_name_of_command_name name = Printf.sprintf "%s-%s" infer_exe_name name

let exe_name_of_command cmd = name_of_command cmd |> exe_name_of_command_name

let command_of_exe_name exe_name =
  List.find_map command_to_name ~f:(fun (cmd, name) ->
      if String.equal exe_name (exe_name_of_command_name name) then Some cmd else None )

let mk_command_doc ~see_also:see_also_commands ?and_also ?environment:environment_opt
    ?files:files_opt ~synopsis =
  let section = 1 in
  let see_also =
    let exe_names =
      List.map see_also_commands ~f:(fun cmd ->
          let exe = exe_name_of_command cmd in
          Printf.sprintf "$(b,%s)(%d)" (Cmdliner.Manpage.escape exe) section )
    in
    let suffix = Option.value ~default:"" and_also in
    [`P (String.concat ~sep:", " exe_names ^ suffix)]
  in
  let environment =
    Option.value environment_opt
      ~default:
        [ `I
            ( Printf.sprintf "$(b,%s), $(b,%s), $(b,%s)" CLOpt.args_env_var inferconfig_env_var
                CLOpt.strict_mode_env_var
            , Printf.sprintf "See the %s section in the manual of $(b,infer)(%d)."
                Cmdliner.Manpage.s_environment section ) ]
  in
  let files =
    Option.value files_opt
      ~default:
        [ `I
            ( Printf.sprintf "$(b,%s)" inferconfig_file
            , Printf.sprintf "See the %s section in the manual of $(b,infer)(%d)."
                Cmdliner.Manpage.s_files section ) ]
  in
  CLOpt.mk_command_doc ~section ~version:Version.versionString
    ~date:Version.man_pages_last_modify_date ~synopsis:[`Pre synopsis] ~environment ~files
    ~see_also

let analyze =
  mk_command_doc ~title:"Infer Analysis" ~short_description:"analyze the files captured by infer"
    ~synopsis:{|$(b,infer) $(b,analyze) $(i,[options])
$(b,infer) $(i,[options])|}
    ~description:[`P "Analyze the files captured in the project results directory and report."]
    ~see_also:CLOpt.([Report; Run])

let capture =
  mk_command_doc ~title:"Infer Compilation Capture"
    ~short_description:"capture source files for later analysis"
    ~synopsis:
      {|$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,buck) $(i,...)
$(b,infer) $(b,capture) $(b,--flavors) $(i,[options]) $(b,--) $(b,buck) $(i,...)
$(b,infer) $(b,capture) $(b,--buck-compilation-database) $(i,[no-]deps) $(i,[options]) $(b,--) $(b,buck) $(i,...)
$(b,infer) $(b,capture) $(i,[options]) $(b,--compilation-database) $(i,file)
$(b,infer) $(b,capture) $(i,[options]) $(b,--compilation-database-escaped) $(i,file)
$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,gradle)/$(b,gradlew) $(i,...)
$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,javac) $(i,...)
$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,make)/$(b,clang)/$(b,gcc) $(i,...)
$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,mvn)/$(b,mvnw) $(i,...)
$(b,infer) $(b,capture) $(i,[options]) $(b,--) $(b,ndk-build) $(i,...)
$(b,infer) $(b,capture) $(i,[--no-xcpretty]) $(i,[options]) $(b,--) $(b,xcodebuild) $(i,...)|}
    ~description:
      [ `P
          "Capture the build command or compilation database specified on the command line: infer intercepts calls to the compiler to read source files, translate them into infer's intermediate representation, and store the result of the translation in the results directory."
      ]
    ~see_also:CLOpt.([Analyze; Compile; Run])

let compile =
  mk_command_doc ~title:"Infer Project Compilation"
    ~short_description:"compile project from within the infer environment"
    ~synopsis:"$(b,infer) $(b,compile) $(b,--) $(i,[compile command])"
    ~description:
      [ `P
          "Intercepts compilation commands similarly to $(b,infer-capture), but simply execute these compilation commands and do not perform any translation of the source files. This can be useful to configure build systems or for debugging purposes."
      ]
    ~examples:
      [ `P
          "$(b,cmake)(1) hardcodes the absolute paths to the compiler inside the Makefiles it generates, which defeats the later capture of compilation commands by infer. Thus, to capture a CMake project, one should configure the project from within the infer build environment, for instance:"
      ; `Pre {|  mkdir build && cd build
  infer compile -- cmake ..
  infer capture -- make|}
      ; `P
          "The same solution can be used for projects whose \"./configure\" script hardcodes the paths to the compilers, for instance:"
      ; `Pre {|  infer compile -- ./configure
  infer capture -- make|}
      ; `P
          "Another solution for CMake projects is to use CMake's compilation databases, for instance:"
      ; `Pre
          {|  mkdir build && cd build
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
  infer capture --compilation-database compile_commands.json|}
      ]
    ~see_also:CLOpt.([Capture])

let diff =
  mk_command_doc ~title:"Infer Differential Analysis of a Project"
    ~short_description:"Report the difference between two versions of a project"
    ~synopsis:"$(b,infer) $(b,diff) $(i,[options])"
    ~description:[`P "EXPERIMENTAL AND IN NO WAY READY TO USE"]
    ~see_also:CLOpt.([ReportDiff; Run])

let infer =
  mk_command_doc ~title:"Infer Static Analyzer"
    ~short_description:"static analysis for Java and C/C++/Objective-C/Objective-C++"
    ~synopsis:
      {|$(b,infer) $(b,analyze) $(i,[options])
$(b,infer) $(b,capture) $(i,[options])
$(b,infer) $(b,compile) $(i,[options])
$(b,infer) $(b,report) $(i,[options])
$(b,infer) $(b,reportdiff) $(i,[options])
$(b,infer) $(b,run) $(i,[options])
$(b,infer) $(b,--compilation-database[-escaped]) $(i,file) $(i,[options])
$(b,infer) $(i,[options]) $(b,--) $(b,compile command)
$(b,infer) $(i,[options])|}
    ~description:
      [ `P
          "Infer is a static analyzer. Given a collection of source files written in Java or in languages of the C family, and a command to build them, infer produces a list of potential issues."
      ; `P
          "Infer consists of a collection of tools referenced in the $(i,SEE ALSO) section of this manual. See their respective manuals for more information about each."
      ]
    ~options:
      (`Prepend
        [ `P
            "If a compilation command is specified via the $(b,--) option or one of the $(b,--clang-compilation-database[-escaped]) options, $(b,infer) behaves as $(b,infer-run)(1). Otherwise, $(b,infer) behaves as $(b,infer-analyze)(1)."
        ; `P
            "Every infer command accepts the arguments from all the other infer commands. The same option may affect and thus be list in the manual of several commands."
        ; `P
            (Printf.sprintf
               "Options are read from the $(b,%s) file, then from the $(b,%s) environment variable, then from the command line. Options in $(b,%s) take precedence over options in $(b,%s), and options passed on the command line take precedence over options in $(b,%s). See the $(i,%s) and $(i,%s) sections of this manual for more information."
               inferconfig_file CLOpt.args_env_var CLOpt.args_env_var inferconfig_file
               CLOpt.args_env_var Cmdliner.Manpage.s_environment Cmdliner.Manpage.s_files)
        ; `P
            "Options can be specified inside an argument file $(i,file) by passing $(b,@)$(i,file) as argument. The format is one option per line, and enclosing single ' and double \" quotes are ignored."
        ; `P
            "See the manuals of individual infer commands for details about their supported options. The following is a list of all the supported options (see also $(b,--help-full) for options reserved for internal use)."
        ])
    ~environment:
      [ `P
          (Printf.sprintf
             "Extra arguments may be passed to all infer commands using the $(b,%s) environment variable (see the $(i,%s) section). $(b,%s) is expected to contain a string of %c-separated options. For instance, calling `%s=--debug^--print-logs infer` is equivalent to calling `infer --debug --print-logs`."
             CLOpt.args_env_var Cmdliner.Manpage.s_options CLOpt.args_env_var CLOpt.env_var_sep
             CLOpt.args_env_var)
      ; `P
          (Printf.sprintf "$(b,%s): Tells infer where to find the %s file. (See the %s section)"
             inferconfig_env_var inferconfig_file Cmdliner.Manpage.s_files)
      ; `P
          (Printf.sprintf
             "If $(b,%s) is set to \"1\", then infer commands will exit with an error code in some cases when otherwise a simple warning would be emitted on stderr, for instance if a deprecated form of an option is used."
             CLOpt.strict_mode_env_var) ]
    ~files:
      [ `P
          (Printf.sprintf
             "$(b,%s) can be used to store infer options. Its format is that of a JSON record, where fields are infer long-form options, without their leading \"--\", and values depend on the type of the option:"
             inferconfig_file)
      ; `Noblank
      ; `P "- for switches options, the value is a JSON boolean (true or false, without quotes)"
      ; `Noblank
      ; `P "- for integers, the value is a JSON integer (without quotes)"
      ; `Noblank
      ; `P "- string options have string values"
      ; `Noblank
      ; `P
          (Printf.sprintf
             "- path options have string values, and are interpreted relative to the location of the %s file"
             inferconfig_file)
      ; `Noblank
      ; `P "- cumulative options are JSON arrays of the appropriate type"
      ; `P
          (Printf.sprintf
             "Infer will look for an $(b,%s) file in the current directory, then its parent, etc., stopping at the first $(b,%s) file found."
             inferconfig_file inferconfig_file)
      ; `P "Example:"
      ; `Pre
          {|  {
    "cxx": false,
    "infer-blacklist-files-containing": ["@generated","@Generated"]
  }|}
      ]
    ~see_also:(List.filter ~f:(function CLOpt.Clang -> false | _ -> true) CLOpt.all_commands)
    ~and_also:", $(b,inferTraceBugs)" "infer"

let report =
  mk_command_doc ~title:"Infer Reporting" ~short_description:"compute and manipulate infer results"
    ~synopsis:"$(b,infer) $(b,report) $(i,[options]) [$(i,file.specs)...]"
    ~description:
      [ `P
          "Read, convert, and print .specs files in the results directory. Each spec is printed to standard output unless option -q is used."
      ; `P
          "If no specs file are passed on the command line, process all the .specs in the results directory."
      ]
    ~see_also:CLOpt.([ReportDiff; Run])

let reportdiff =
  mk_command_doc ~title:"Infer Report Difference"
    ~short_description:"compute the differences between two infer reports"
    ~synopsis:
      "$(b,infer) $(b,reportdiff) $(b,--report-current) $(i,file) $(b,--report-previous) $(i,file) $(i,[options])"
    ~description:
      [ `P
          "Given two infer reports $(i,previous) and $(i,current), compute the following three reports and store them inside the \"differential/\" subdirectory of the results directory:"
      ; `Noblank
      ; `P
          "- $(b,introduced.json) contains the issues found in $(i,current) but not $(i,previous);"
      ; `Noblank
      ; `P "- $(b,fixed.json) contains the issues found in $(i,previous) but not $(i,current);"
      ; `Noblank
      ; `P
          "- $(b,preexisting.json) contains the issues found in both $(i,previous) and $(i,current)."
      ; `P "All three files follow the same format as normal infer reports." ]
    ~see_also:CLOpt.([Report])

let run =
  mk_command_doc ~title:"Infer Analysis of a Project"
    ~short_description:"capture source files, analyze, and report"
    ~synopsis:
      {|$(b,infer) $(b,run) $(i,[options])
$(b,infer) $(i,[options]) $(b,--) $(i,compile command)|}
    ~description:
      [ `P
          "Calling \"$(b,infer) $(b,run) $(i,[options])\" is equivalent to performing the following sequence of commands:"
      ; `Pre {|$(b,infer) $(b,capture) $(i,[options])
$(b,infer) $(b,analyze) $(i,[options])|} ]
    ~see_also:CLOpt.([Analyze; Capture; Report])

let command_to_data =
  let mk cmd mk_doc =
    let name = name_of_command cmd in
    let command_doc = mk_doc (exe_name_of_command cmd) in
    (cmd, {name; command_doc})
  in
  let open CLOpt in
  [ mk Analyze analyze
  ; mk Capture capture
  ; mk Compile compile
  ; mk Diff diff
  ; mk Report report
  ; mk ReportDiff reportdiff
  ; mk Run run ]

let data_of_command command =
  List.Assoc.find_exn ~equal:CLOpt.equal_command command_to_data command
