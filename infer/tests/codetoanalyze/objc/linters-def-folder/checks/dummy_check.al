// Copyright (c) 2017 - present Facebook, Inc.
// All rights reserved.
//
// This source code is licensed under the BSD style license found in the
// LICENSE file in the root directory of this source tree. An additional grant
// of patent rights can be found in the PATENTS file in the same directory.

DEFINE-CHECKER FORBIDDEN_NAME_EXAMPLE = {
    SET report_when = WHEN is_class("ForbiddenClassName") HOLDS-IN-NODE ObjCInterfaceDecl;
    SET message = "ForbiddenClassName is forbidden, please use another name";
};
