(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** This module handles C or Objective-C types for which there are special rules for memory management *)

(** This module models special c struct types from the Apple's Core Foundation libraries
    for which there are particular rules for memory management. *)

module Core_foundation_model = struct
  let core_foundation =
    [ "__CFArray"
    ; "__CFAttributedString"
    ; "__CFBag"
    ; "__CFNull"
    ; "__CFAllocator"
    ; "__CFBinaryHeap"
    ; "__CFBitVector"
    ; "__CFBundle"
    ; "__CFCalendar"
    ; "__CFCharacterSet"
    ; "__CFDate"
    ; "__CFDateFormatter"
    ; "__CFDictionary"
    ; "__CFError"
    ; "__CFFileDescriptor"
    ; "__CFFileSecurity"
    ; "__CFLocale"
    ; "__CFMachPort"
    ; "__CFMessagePort"
    ; "__CFNotificationCenter"
    ; "__CFBoolean"
    ; "__CFNumber"
    ; "__CFNumberFormatter"
    ; "__CFPlugInInstance"
    ; "__CFReadStream"
    ; "__CFWriteStream"
    ; "__CFRunLoop"
    ; "__CFRunLoopSource"
    ; "__CFRunLoopObserver"
    ; "__CFRunLoopTimer"
    ; "__CFSet"
    ; "__CFStringTokenizer"
    ; "__CFSocket"
    ; "__CFReadStream"
    ; "__CFWriteStream"
    ; "__CFTimeZone"
    ; "__CFTree"
    ; "__CFURLEnumerator"
    ; "__CFUUID" ]

  let cf_network =
    [ "_CFHTTPAuthentication"
    ; "__CFHTTPMessage"
    ; "__CFHost"
    ; "__CFNetDiagnostic"
    ; "__CFNetService"
    ; "__CFNetServiceMonitor"
    ; "__CFNetServiceBrowser" ]

  let core_media =
    [ "OpaqueCMBlockBuffer"
    ; "opaqueCMBufferQueue"
    ; "opaqueCMBufferQueueTriggerToken"
    ; "opaqueCMFormatDescription"
    ; "OpaqueCMMemoryPool"
    ; "opaqueCMSampleBuffer"
    ; "opaqueCMSimpleQueue"
    ; "OpaqueCMClock"
    ; "OpaqueCMTimebase" ]

  let core_text =
    [ "__CTFont"
    ; "__CTFontCollection"
    ; "__CTFontDescriptor"
    ; "__CTFrame"
    ; "__CTFramesetter"
    ; "__CTGlyphInfo"
    ; "__CTLine"
    ; "__CTParagraphStyle"
    ; "__CTRubyAnnotation"
    ; "__CTRun"
    ; "__CTRunDelegate"
    ; "__CTTextTab"
    ; "__CTTypesetter" ]

  let core_video =
    ["__CVBuffer"; "__CVMetalTextureCache"; "__CVOpenGLESTextureCache"; "__CVPixelBufferPool"]

  let image_io = ["CGImageDestination"; "CGImageMetadata"; "CGImageMetadataTag"; "CGImageSource"]

  let security =
    [ "__SecCertificate"
    ; "__SecIdentity"
    ; "__SecKey"
    ; "__SecPolicy"
    ; "__SecAccessControl"
    ; "__SecRandom"
    ; "__SecCode"
    ; "__SecTrust"
    ; "__SecRequirement" ]

  let system_configuration =
    [ "__SCDynamicStore"
    ; "__SCNetworkInterface"
    ; "__SCBondStatus"
    ; "__SCNetworkProtocol"
    ; "__SCNetworkService"
    ; "__SCNetworkSet"
    ; "__SCNetworkConnection"
    ; "__SCNetworkReachability"
    ; "__SCPreferences" ]

  let core_graphics_types =
    [ "CGAffineTransform"
    ; "CGBase"
    ; "CGBitmapContext"
    ; "CGColor"
    ; "CGColorSpace"
    ; "CGContext"
    ; "CGDataConsumer"
    ; "CGDataProvider"
    ; "CGError"
    ; "CGFont"
    ; "CGFunction"
    ; "CGGeometry"
    ; "CGGradient"
    ; "CGImage"
    ; "CGLayer"
    ; "CGPath"
    ; "CGPattern"
    ; "CGPDFArray"
    ; "CGPDFContentStream"
    ; "CGPDFContext"
    ; "CGPDFDictionary"
    ; "CGPDFDocument"
    ; "CGPDFObject"
    ; "CGPDFOperatorTable"
    ; "CGPDFPage"
    ; "CGPDFScanner"
    ; "CGPDFStream"
    ; "CGPDFString"
    ; "CGShading" ]

  let core_foundation_types =
    core_foundation @ cf_network @ core_media @ core_text @ core_video @ image_io @ security
    @ system_configuration

  let copy = "Copy"

  let create = "Create"

  let cf_retain = "CFRetain"

  let cf_release = "CFRelease"

  let upper_release = "Release"

  let ref = "Ref"

  let cf_type = "CFTypeRef"

  type core_lib = Core_foundation | Core_graphics

  let core_lib_to_type_list lib =
    match lib with
    | Core_foundation
     -> core_foundation_types
    | Core_graphics
     -> core_graphics_types

  let is_objc_memory_model_controlled o =
    List.mem ~equal:String.equal core_foundation_types o
    || List.mem ~equal:String.equal core_graphics_types o

  let rec is_core_lib lib typ =
    match typ.Typ.desc with
    | Typ.Tptr (styp, _)
     -> is_core_lib lib styp
    | Typ.Tstruct name
     -> let core_lib_types = core_lib_to_type_list lib in
        List.mem ~equal:String.equal core_lib_types (Typ.Name.name name)
    | _
     -> false

  let is_core_foundation_type typ = is_core_lib Core_foundation typ

  let is_core_graphics_type typ = is_core_lib Core_graphics typ

  let is_core_lib_type typ = is_core_foundation_type typ || is_core_graphics_type typ

  let is_core_lib_create typ funct =
    is_core_lib_type typ
    && (String.is_substring ~substring:create funct || String.is_substring ~substring:copy funct)

  let function_arg_is_cftype typ = String.is_substring ~substring:cf_type typ

  let is_core_lib_retain typ funct = function_arg_is_cftype typ && String.equal funct cf_retain

  let is_core_lib_release typ funct = function_arg_is_cftype typ && String.equal funct cf_release

  let is_core_graphics_release typ funct =
    let f lib =
      String.equal funct (lib ^ upper_release) && String.is_substring ~substring:(lib ^ ref) typ
    in
    List.exists ~f core_graphics_types

  (*
  let function_arg_is_core_pgraphics typ =
    let res = (String.is_substring ~substring:cf_type typ) in
    res
*)
end

let is_core_lib_type typ = Core_foundation_model.is_core_lib_type typ
