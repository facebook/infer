// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

GLOBAL-MACROS {

  LET imported_is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

};


GLOBAL-PATHS {
 LET all_files = {REGEXP(".*") };
};
