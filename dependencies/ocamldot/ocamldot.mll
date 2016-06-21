(* ocamldot.mll, July 1999, Trevor Jim *)

{
module StringSet =
  Set.Make(struct type t = string let compare = compare end)

let dependencies = ref []
let currentSource = ref ""
let addDepend t =
  let s = !currentSource in
  if s<>t
  then dependencies := (s,t)::(!dependencies)

}

rule processSource = parse
    ['.' '-' '/' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246'
     '\248'-'\255' '\'' '0'-'9' ]+ '.' ['A'-'Z' 'a'-'z']+
    [' ' '\009']* ':'
      { let s = Lexing.lexeme lexbuf in
        let i = String.rindex s '.' in
        let s = String.sub s 0 i in
        let s = Filename.basename s in
        let s = String.capitalize s in
        currentSource := s;
        processTargets lexbuf }
  | eof
      { () }
  | _
      { processSource lexbuf }

and processTargets = parse
    [' ' '\009']+
      { processTargets lexbuf }
  | '\\' [' ' '\009']* ['\010' '\013']+ [' ' '\009']+
      { processTargets lexbuf }
  | ['.' '/' 'A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246'
     '\248'-'\255' '\'' '0'-'9' ]+ '.' ['A'-'Z' 'a'-'z']+
      { let t = Lexing.lexeme lexbuf in
        let i = String.rindex t '.' in
        let t = String.sub t 0 i in
        let t = Filename.basename t in
        let t = String.capitalize t in
        addDepend t;
        processTargets lexbuf }
  | eof
      { () }
  | _
      { processSource lexbuf }

{

                  (********************************)
                  (* Utility functions for graphs *)
                  (********************************)


(**********************************************************************)
(* A graph is represented by a (string * StringSet) list,             *)
(* that is, a list of (source,targets) pairs.                         *)
(**********************************************************************)

let emptyGraph = []

(**********************************************************************)
(* divideGraph graph source = (sourceTargets, graphWithoutSource)     *)
(*                                                                    *)
(* Return the targets of a source in a graph and the graph with the   *)
(* source substracted from the sources.  GraphWithoutSources may      *)
(* still contain source as a target.                                  *)
(**********************************************************************)
let divideGraph graph source =
  let rec aux l =
    match l with
      [] -> (StringSet.empty,[])
    | (s,ts)::tl ->
        if s=source then (ts,tl)
        else
          let (sourceTargets,tlWithoutSource) = aux tl in
          (sourceTargets,(s,ts)::tlWithoutSource) in
  aux graph

(*********************************************)
(* Add the edge (source,target) to the graph *)
(*********************************************)
let addEdge graph source target =
  let (sourceTargets,graphWithoutSource) = divideGraph graph source in
  (source,StringSet.add target sourceTargets)::graphWithoutSource

(************************************************************)
(* Add the edges { (source,t) | t in targets } to the graph *)
(************************************************************)
let addEdges graph source targets =
  let (sourceTargets,graphWithoutSource) = divideGraph graph source in
  (source,StringSet.union targets sourceTargets)::graphWithoutSource

(**************************************************)
(* Remove the edge (source,target) from the graph *)
(**************************************************)
let removeEdge graph source target =
  let rec loop l =
    match l with
      [] -> []
    | (s,ts)::tl ->
        if s=source
        then (s,StringSet.remove target ts)::tl
        else (s,ts)::(loop tl)
  in loop graph

(*****************************************************************)
(* Remove the edges { (source,t) | t in targets } from the graph *)
(*****************************************************************)
let removeEdges graph source targets =
  let rec loop l =
    match l with
      [] -> []
    | (s,ts)::tl ->
        if s=source
        then (s,StringSet.diff ts targets)::tl
        else (s,ts)::(loop tl)
  in loop graph

(**********************************************************************)
(* Convert between an edge-list representation of graphs and our      *)
(* representation.                                                    *)
(**********************************************************************)
let edgesOfGraph graph =
  List.concat
    (List.map
       (fun (s,ts) ->
         List.map (fun t -> (s,t)) (StringSet.elements ts))
       graph)

let graphOfEdges edges =
  List.fold_left
    (fun g (s,t) -> addEdge g s t)
    emptyGraph
    edges

(****************************)
(* Is an edge in the graph? *)
(****************************)
let isEdge graph source target =
  try
    let sourceTargets = List.assoc source graph in
    StringSet.mem target sourceTargets
  with Not_found -> false

(*****************)
(* Print a graph *)
(*****************)
let printGraph graph =
  let printEdges(source,targets) =
    StringSet.iter
      (fun t -> Printf.printf "  \"%s\" -> \"%s\" ;\n" source t)
      targets in
  List.iter printEdges graph

(********************************)
(* Targets of a node in a graph *)
(********************************)
let targetsOf graph node = (* A set of nodes *)
  try List.assoc node graph
  with Not_found -> StringSet.empty

(*****************************************)
(* Sources that target a node in a graph *)
(*****************************************)
let sourcesOf graph node = (* A list of nodes *)
  let rec aux l =
    match l with
      [] -> []
    | (s,ts)::tl ->
        if StringSet.mem node ts then s::(aux tl)
        else aux tl in
  aux graph

(******************************************************************)
(* Add an edge to a transitively closed graph, and return the new *)
(* transitive closure.                                            *)
(******************************************************************)
let addEdgeTc graph source target =
  let targetTargets = targetsOf graph target in
  let (sourceTargets,graphWithoutSource) = divideGraph graph source in
  let sourceSources = sourcesOf graphWithoutSource source in
  let newSourceTargets =
    StringSet.add target
      (StringSet.union sourceTargets targetTargets) in
  (source,newSourceTargets)::
  (List.fold_right
     (fun s g -> addEdges g s newSourceTargets)
     sourceSources
     graphWithoutSource)

(**********************************************************)
(* Compute the transitive closure of a graph from scratch *)
(**********************************************************)
let tc graph =
  let loop graph (source,targets) =
    let reachableFromSource =
      List.fold_left
        (fun r (s,ts) ->
          if StringSet.mem s r then StringSet.union r ts
          else r)
        targets
        graph in
    (source,reachableFromSource)::
    (List.map
       (fun (s,ts) ->
         if StringSet.mem source ts
         then (s,StringSet.union ts reachableFromSource)
         else (s,ts))
      graph) in
  List.fold_left loop [] graph

(************************************************************************)
(* The transitive kernel (tk) of a dag is a subset of the dag whose     *)
(* transitive closure is the same as the transitive closure of the dag. *)
(*                                                                      *)
(* IF THE GRAPH IS NOT A DAG, THIS CODE WON'T WORK PROPERLY!!!          *)
(************************************************************************)

(************************************************************************)
(* Add an edge to a kernel dag and return the new kernel and transitive *)
(* closure of the new kernel.  Requires the transitive closure of the   *)
(* old kernel.                                                          *)
(************************************************************************)
let addEdgeTk kernel tcKernel source target =
  if isEdge tcKernel source target
  then (kernel,tcKernel)
  else if source=target
  then (addEdge kernel source target,tcKernel)
  else
  begin
    let (sourceTargets,kernelWithoutSource) = divideGraph kernel source in
    let targetTargets = StringSet.add target (targetsOf tcKernel target) in
    let sourceSources = sourcesOf tcKernel source in
    let kernelWithoutSource =
      List.fold_left
        (fun kws s -> removeEdges kws s targetTargets)
        kernelWithoutSource
        sourceSources in
    ((source,
      StringSet.add target
        (StringSet.diff sourceTargets targetTargets))
      ::kernelWithoutSource,
     addEdgeTc tcKernel source target)
  end

(**********************************)
(* The transitive kernel of a dag *)
(**********************************)
let tk dag =
  let edges = edgesOfGraph dag in
  let (kernel,tcKernel) =
    List.fold_left
      (fun (k,tck) (s,t) -> addEdgeTk k tck s t)
      (emptyGraph,emptyGraph)
      edges in
  kernel

(**************************)
(* Print the dependencies *)
(**************************)
let doKernel = ref true
let printDepend graph =
  if (!doKernel) then printGraph (tk graph)
  else printGraph graph

let calledOnFile = ref false
let getDependFromFile file =
  calledOnFile := true;
  try
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    processSource lexbuf;
    close_in ic
  with Sys_error msg -> ()
  | Exit -> ()
let getDependFromStdin () =
  try
    let lexbuf = Lexing.from_channel stdin in
    processSource lexbuf
  with Sys_error msg -> ()
  | Exit -> ()

                          (***************)
                          (* Entry point *)
                          (***************)

let usage = "Usage: ocamldot [options] <files>"

let leftToRight = ref false
let landscape = ref false
let roots = ref []
;;

Arg.parse
  [
    ("-fullgraph",
     Arg.Clear doKernel,
     "  draw the full graph (default is to draw only the kernel)");
    ("-landscape",
     Arg.Set landscape,
     "  output in landscape format (default is portrait)");
    ("-lr",
     Arg.Set leftToRight,
     "         draw graph from left to right (default is top to bottom)");
    ("-r",
     Arg.String(fun s -> roots := s::!roots),
     "<r>       use <r> as a root in the graph; nodes reachable from <r>\n               will be shown")
  ]
  getDependFromFile usage;
if not(!calledOnFile) then getDependFromStdin();
print_string "digraph G {\n";
if !landscape
then print_string "  size=\"10,7.5\" ;\n  rotate=90 ;\n"
else print_string "  size=\"7.5,10\" ;\n";
if (!leftToRight) then print_string "  rankdir = LR ;\n"
else print_string "  rankdir = TB ;\n";
let graph = graphOfEdges(!dependencies) in
begin
  match !roots with
    [] -> printDepend graph
  | roots ->
    (* Set up the graph so that the roots are printed at the same level *)
    print_string "  { rank=same ;\n";
    List.iter
      (fun r ->
         print_string "    ";
         print_string r;
         print_string " ;\n")
      roots;
    print_string "  };\n";
    (* Find the graph reachable from the roots *)
    let tcGraph = tc graph in          
    let reachable node =
      (List.exists (fun r -> r=node) roots)
      or
      (List.exists (fun r -> isEdge tcGraph r node) roots) in
    let reachableFromRoots =
      List.concat
        (List.map
           (fun (source,targets) ->
             if reachable source
             then [(source,targets)]
             else [])
           graph) in
    printDepend reachableFromRoots
end;
print_string "}\n";
exit 0
;;

}
