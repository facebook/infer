(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

{
  open JVerboseParser
}

let space = [' ' '\t']

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']
let letter = lowerletter | upperletter

let underscore = '_'
let minus = '-'
let dot = '.'
let hash = '#'
let dollar = '$'
let equal = '='
let tilde = '~'

let digit = ['0'-'9']
let number = digit* | digit+ '.' digit* | digit* '.' digit+
let char = letter | digit

let dir_sep = '/'

let dot_java = dot "java"
let dot_class = dot "class"
let path = (char | dir_sep | underscore | minus | dot | hash | dollar | equal | tilde)+
let source_basename = path dot_java
let class_basename = path dot_class
let source_filename = (dir_sep path | path)* source_basename
let class_filename = (dir_sep path | path)* class_basename

let search_path = "search path for class files"

rule token = parse
| [' ' '\t']                { token lexbuf }     (* skip blanks *)
| ['\n']                    { EOL }
| '['                       { LEFT_SQUARE_BRACKET }
| ']'                       { RIGHT_SQUARE_BRACKET }
| ':'                       { COLON }
| ','                       { COMMA }
| "\'\'"                    { EMPTY }
| "parsing"                 { PARSING }
| "started"                 { STARTED }
| "wrote"                   { WROTE }
| search_path               { SEARCH_PATH }
| "RegularFileObject"       { REGULARFILEOBJECT }
| "ZipFileIndexFileObject"  { ZIPFILEINDEXFILEOBJECT }
| source_filename as p      { SOURCE_FILENAME p }
| class_filename as p       { CLASS_FILENAME p }
| path as p                 { PATH p }
| eof                       { EOF }
