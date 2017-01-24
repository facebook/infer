/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** The Smallfoot Intermediate Language: Call Flags */
let module L = Logging;

let module F = Format;


/** Flags for a procedure call */
type t = {
  cf_virtual: bool,
  cf_interface: bool,
  cf_noreturn: bool,
  cf_is_objc_block: bool,
  cf_targets: list Procname.t
}
[@@deriving compare];

let pp f cf => {
  if cf.cf_virtual {
    F.fprintf f " virtual"
  };
  if cf.cf_noreturn {
    F.fprintf f " noreturn"
  }
};

let default = {
  cf_virtual: false,
  cf_interface: false,
  cf_noreturn: false,
  cf_is_objc_block: false,
  cf_targets: []
};
