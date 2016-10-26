/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
/* module for escaping clang arguments on the command line and put them into files */

/** quoting style */
type style =
  | EscapedDoubleQuotes /** the arguments should be enclosed in "double quotes" and are already escaped */
  | SingleQuotes /** the arguments should be enclosed in 'single quotes' and have to be escaped */;

let quote: style => string => string;

let mk_arg_file: string => style => list string => string;
