# Intermediate Representation

The Intermediate Representation is a format used by the back-end for analysis. It is produced by one of the front-ends, one for each program analyzed.

The main entry point is the intermediate language in [Sil](Sil.rei).

The control flow graph module is [Cfg](Cfg.rei).

The call graph module is [Cg](Cg.rei).

The type environment module is [Tenv](Tenv.rei).

## Cg

The Call Graph module ([Cg](Cg.re)) contains the following attributes in the Call Graph type:

  - Path to source file
  - Number of lines of code in source file
  - Map of Procedure Hash to Procedure Info (referred as Procname.Hash and node_info respectively)

Procedure Info consists of the following attributes:

  - Parent Procedures
  - Children Procedures
  - Ancestors
  - Heirs
  - Recursive Dependents
  - Number of in-calls and out-calls

- `get_calls` computes the calls of the Procedure.
- `store_to_file` saves the call graph in a file.
- `load_from_file` loads a call graph from a file.
- `extend` extends an old call graph in-place with nodes and edges from a new call graph.

## Cfg

The Control Flow Graph module ([Cfg](Cfg.re)) defines the `cfg` data-type, which contains the following information:

  - Node ID
  - List of Nodes
  - Map of Procedure Hash to Procedure Description (proc_desc)
  - Priority Set of Procedures (functions to be analyzed first)

Module `proc_desc` represents Procedure Description and contains the following information:

  - Attributes of this Procedure (`pd_attributes`)
  - Unique Process Identifier (`pd_id`)
  - List of nodes of the procedure (`pd_nodes`)
  - Start node of the procedure (`pd_start_node`)
  - Exit node of the procedure (`pd_exit_node`)

Module `Node` represents a node of the procedure and contains the following information:

  - Unique ID of the node
  - Distance to the exit node
  - Dead program variables before executing the instructions
  - Dead program variables after executing the instructions
  - Exception nodes in the CFG
  - Instructions for symbolic execution
  - Kind of node (Kinds: Start Node, Exit Node, Statement Node, Join Node, Prune Node, Skip Node)
  - Location in the procedure
  - Predecessor nodes in the CFG
  - Successor nodes in the CFG
  - Procedure Description

- `store_cfg_to_file` saves a CFG into a file.
- `load_cfg_from_file` loads a CFG from a file.
