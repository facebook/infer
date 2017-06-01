// Copyright (c) 2017 - present Facebook, Inc.
// All rights reserved.
//
// This source code is licensed under the BSD style license found in the
// LICENSE file in the root directory of this source tree. An additional grant
// of patent rights can be found in the PATENTS file in the same directory.

DEFINE-CHECKER SUBCLASSING_TEST_EXAMPLE = {
    SET report_when = is_class("A") HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;
    SET message = "This is subclassing A. Class A should not be subclassed.";
};
