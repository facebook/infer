
GLOBAL-MACROS {

  LET imported_is_subclass_of(x) =
        is_class(x) HOLDS-IN-SOME-SUPERCLASS-OF ObjCInterfaceDecl;

};
