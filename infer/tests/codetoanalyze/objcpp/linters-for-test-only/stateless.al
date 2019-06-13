// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// Requirements for stateless component implemented in this rule:
// 1. Subclass of CKCompositeComponent
// 2. Has only 1 method, which is constructor class method newWith...
// 3. Does not conform to any protocols
// 4. Does not have any ivars
// 5. Does not have any properties
// 6. Does not have any methods
// 7. Does not have a view configuration. Which means, it should only have a call to either of:
//   a. [super(or self) newWithComponent: ....];
//   b. [super(or self) newWithView:{} component: ...];
// 8. Does not declare a scope in the constructor: which means does call *CKComponentScope *nameOfLocalScopeVariable(....);
DEFINE-CHECKER CKSTATELESS_COMPONENT = {

 LET is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

 LET receiver_is_super_or_self = is_receiver_super() OR is_receiver_self();

 LET eventually_call_newWithComponent =
  IN-NODE ObjCMethodDecl WITH-TRANSITION Body
     ((call_method(REGEXP("newWithComponent:.*")) AND receiver_is_super_or_self) HOLDS-EVENTUALLY)
   HOLDS-EVENTUALLY;


  // ObjCMessageExpr -> MaterializeTemporaryExpr -> CXXBindTemporaryExpr -> CXXConstructExpr[]
  LET view_is_empty =
   WHEN
   (is_node("MaterializeTemporaryExpr") AND
    (is_node("CXXBindTemporaryExpr") AND
      (cxx_construct_expr_has_no_parameters() HOLDS-NEXT)
    ) HOLDS-NEXT
   ) HOLDS-NEXT
   HOLDS-IN-NODE ObjCMessageExpr;

   LET eventually_call_newWithViewcomponent =
    IN-NODE ObjCMethodDecl WITH-TRANSITION Body
       ((call_method(REGEXP("newWithView:component:.*")) AND receiver_is_super_or_self
         AND
         view_is_empty)
          HOLDS-EVENTUALLY)
     HOLDS-EVENTUALLY;


  LET is_scope_declaration =
   WHEN
     (cxx_construct_expr_has_name("CKComponentScope") HOLDS-NEXT)
   HOLDS-IN-NODE DeclStmt;

  LET eventually_declare_scope =
   IN-NODE ObjCMethodDecl WITH-TRANSITION Body
      (is_scope_declaration HOLDS-EVENTUALLY)
    HOLDS-EVENTUALLY;


  LET implementation_requirements =
  WHEN
   objc_class_has_only_one_constructor_method_named(REGEXP("newWith.+"))
   AND (NOT (is_objc_instance_method_named(REGEXP(".*")) HOLDS-NEXT)) // has not instance methods
   AND (eventually_call_newWithViewcomponent OR eventually_call_newWithComponent) // has view configuration
   AND (NOT eventually_declare_scope)
   AND (NOT (is_node("ObjCIvarDecl") HOLDS-EVENTUALLY))
   AND (NOT (is_node("ObjCPropertyDecl") HOLDS-EVENTUALLY))
 HOLDS-IN-NODE ObjCImplementationDecl;

   LET interface_requirements =
    WHEN
       is_subclass_of("CKCompositeComponent") AND
       (NOT adhere_to_protocol()) AND
      (NOT (is_node("ObjCPropertyDecl") HOLDS-EVENTUALLY))
      AND (NOT (is_node("ObjCIvarDecl") HOLDS-EVENTUALLY))
     HOLDS-IN-NODE ObjCInterfaceDecl;

  SET report_when = WHEN
            INTERFACE [interface_requirements]
            IMPLEMENTATION [implementation_requirements]
         HOLDS-IN-OBJC-CLASS;

  SET message = "This is a CKStateless Component and, for performance reason, can be replaced by a C function";
  SET mode = "ON";

};
