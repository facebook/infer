(*
* Copyright (c) 2013 - Facebook.
* All rights reserved.
*)

module rec CTransImpl : CTrans.CTrans =
  CTrans.CTrans_funct(CMethod_declImpl)

and CMethod_declImpl : CMethod_decl.CMethod_decl =
  CMethod_decl.CMethod_decl_funct(CTransImpl)