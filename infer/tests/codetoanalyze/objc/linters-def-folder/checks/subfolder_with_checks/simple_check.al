// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

DEFINE-CHECKER SUBCLASSING_TEST_EXAMPLE = {
    SET report_when = is_class("A") HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;
    SET message = "This is subclassing A. Class A should not be subclassed.";
};
