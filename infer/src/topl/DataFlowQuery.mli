(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** {1 Data-Flow Query Format}

    This reads "data flow queries" written in a minimal language, and converts them to other
    analyses. The concrete syntax is JSON. The abstract syntax is as follows.

    {v
     QUERY     ::= { "matchers": MATCHERS, "questions" : QUESTIONS, "language": LANGUAGE }
     MATCHERS  ::= [ MATCHER0, MATCHER1, ... ]
     QUESTIONS ::= [ QUESTION0, QUESTION1, ... ]
     MATCHER   ::= { "tag": TAG, "pattern": PATTERN, "arity": ARITY, "position": POSITION }
     QUESTION  ::= { "source": TAG, "sink": TAG }
     POSITION  ::= [ "ret" ]
     POSITION  ::= [ "arg", INDEX ]
    v}

    where TAG is a string, PATTERN is a regex matching procnames, ARITY is a nonnegative integer,
    and INDEX is a 0-based index. A MATCHER has an optional field:

    {v
     MATCHER   ::= {  ..., "condition": [PREDICATE0, PREDICATE1, ...] }
     PREDICATE ::= [ "equal", VALUE, VALUE]
     VALUE     ::= POSITION
     VALUE     ::= CONSTANT
    v}

    The content of CONSTANT varies by LANGUAGE. For example, if LANGUAGE is "erlang", then

    {v
     CONSTANT  ::= [ "atom", STRING ]
     CONSTANT  ::= [ "string", STRING ]
     CONSTANT  ::= [ "int", INTEGER ]
    v}

    As a convenience, one can also specify a MATCHER as a JSON list rather than as a JSON object, in
    which case the list elements are, in order, TAG, PATTERN, ARITY, POSITION, CONDITION. *)

val convert_to_topl : string list -> ToplAst.t list
(** Converts a list of data-flow queries present in the given file paths into Topl properties, one
    Topl property per question. *)
