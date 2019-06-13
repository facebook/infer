// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

DEFINE-CHECKER FORBIDDEN_NAME_EXAMPLE = {
    SET report_when = WHEN is_class("ForbiddenClassName") HOLDS-IN-NODE ObjCInterfaceDecl;
    SET message = "ForbiddenClassName is forbidden, please use another name";
};

DEFINE-CHECKER STRONG_UIAPPLICATION_WARNING = {
    SET report_when =
        WHEN has_type("A *") AND is_strong_ivar()
        HOLDS-IN-NODE ObjCIvarDecl;

    SET message = "Ivar %decl_name% strongly retain A.";
};
