source_filename = "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios15.1.0-simulator"
%objc_class = type { ptr, ptr, ptr, ptr, i64 }
%swift.type_descriptor = type opaque
%swift.protocol_conformance_descriptor = type { i32, i32, i32, i32 }
%swift.async_func_pointer = type <{ i32, i32 }>
%swift.type = type { i64 }
%swift.full_boxmetadata = type { ptr, ptr, %swift.type, i32, ptr }
%swift.full_existential_type = type { ptr, %swift.type }
%swift.method_descriptor = type { i32, i32 }
%swift.protocol_requirement = type { i32, i32 }
%swift.opaque = type opaque
%swift.enum_vwtable = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64, i32, i32, ptr, ptr, ptr }
%swift.vwtable = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64, i32, i32 }
%swift.metadata_response = type { ptr, i64 }
%T16ReproKvoVideoLib24ReproVideoPlayerProviderC = type <{ %swift.refcounted, ptr, %TSo13AVPlayerLayerCSg, %TSo14AVPlayerLooperCSg, %T10Foundation21NSKeyValueObservationCSg, %T16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVSg, { %swift.weak, ptr } }>
%swift.refcounted = type { ptr, i64 }
%TSo13AVPlayerLayerCSg = type <{ [8 x i8] }>
%TSo14AVPlayerLooperCSg = type <{ [8 x i8] }>
%T10Foundation21NSKeyValueObservationCSg = type <{ [8 x i8] }>
%T16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVSg = type <{ [56 x i8] }>
%swift.weak = type { ptr }
%T16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqq_pSg = type <{ [16 x i8] }>
%T16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP = type { ptr, ptr }
%TSo26NSKeyValueObservingOptionsV = type <{ %TSu }>
%TSu = type <{ i64 }>
%TSo20AVPlayerLooperStatusV = type { i64 }
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC12playbackItemAA0c8PlaybackG0OSgvpWvd" = global i64 0, align 8
@"$s10Foundation3URLVMn" = external global %swift.type_descriptor, align 4
@"got.$s10Foundation3URLVMn" = private unnamed_addr constant ptr @"$s10Foundation3URLVMn"
@"symbolic ______AASg5hdUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV" = linkonce_odr hidden constant <{ i8, i32, [44 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s10Foundation3URLVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [44 x i8], i8 }>, ptr @"symbolic ______AASg5hdUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV", i32 0, i32 1) to i64)) to i32), [44 x i8] c"_AASg5hdUrlSb9isLoopingSS16stringIdentifiert", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic SS8manifest______Sg11fallbackUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV" = linkonce_odr hidden constant <{ [12 x i8], i8, i32, [48 x i8], i8 }> <{ [12 x i8] c"SS8manifest_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s10Foundation3URLVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ [12 x i8], i8, i32, [48 x i8], i8 }>, ptr @"symbolic SS8manifest______Sg11fallbackUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV", i32 0, i32 2) to i64)) to i32), [48 x i8] c"Sg11fallbackUrlSb9isLoopingSS16stringIdentifiert", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"\01L_selector_data(isMuted)" = private global [8 x i8] c"isMuted\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(isMuted)" = private externally_initialized global ptr @"\01L_selector_data(isMuted)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@"\01L_selector_data(setMuted:)" = private global [10 x i8] c"setMuted:\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(setMuted:)" = private externally_initialized global ptr @"\01L_selector_data(setMuted:)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@".str.106.vendor/Libraries/FkASqqqqqqq/Capabilities/ReproKvoVideoLib/ReproKvoVideoLib/ReproVideoPlayerProvider.swift" = private unnamed_addr constant [107 x i8] c"vendor/Libraries/FkASqqqqqqq/Capabilities/ReproKvoVideoLib/ReproKvoVideoLib/ReproVideoPlayerProvider.swift\00"
@.str.6.status = private constant [7 x i8] c"status\00"
@"symbolic _____Sg 10Foundation3URLV" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s10Foundation3URLVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg 10Foundation3URLV", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s10Foundation3URLVSgMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 10Foundation3URLV" to i64), i64 ptrtoint (ptr @"$s10Foundation3URLVSgMD" to i64)) to i32), i32 -7 }, align 8
@"symbolic _____Sg 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@".str.18.setupPlayer(item:)" = private unnamed_addr constant [19 x i8] c"setupPlayer(item:)\00"
@"\01L_selector_data(setPlayer:)" = private global [11 x i8] c"setPlayer:\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(setPlayer:)" = private externally_initialized global ptr @"\01L_selector_data(setPlayer:)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@keypath_once = private global i64 0, align 8
@"symbolic So14AVPlayerLooperC" = linkonce_odr hidden constant <{ [19 x i8], i8 }> <{ [19 x i8] c"So14AVPlayerLooperC", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic _____ So20AVPlayerLooperStatusV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo20AVPlayerLooperStatusVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ So20AVPlayerLooperStatusV", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"\01L_selector_data(status)" = private global [7 x i8] c"status\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(status)" = private externally_initialized global ptr @"\01L_selector_data(status)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@keypath = private global <{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 trunc (i64 sub (i64 ptrtoint (ptr @keypath_once to i64), i64 ptrtoint (ptr @keypath to i64)) to i32), i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr getelementptr (i8, ptr @"symbolic So14AVPlayerLooperC", i64 1) to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @keypath, i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr getelementptr (i8, ptr @"symbolic _____ So20AVPlayerLooperStatusV", i64 1) to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @keypath, i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.6.status to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @keypath, i32 0, i32 4) to i64)) to i32), i32 -2147483636, i32 33554433, i32 trunc (i64 sub (i64 ptrtoint (ptr @keypath_get_selector_status to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @keypath, i32 0, i32 7) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo14AVPlayerLooperC6statusSo0aB6StatusVvpABTK" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @keypath, i32 0, i32 8) to i64)) to i32) }>, section "__TEXT,__const", no_sanitize_address, align 8
@"$sSo8NSObjectC10Foundation27_KeyValueCodingAndObservingACWP" = external global ptr, align 8
@"\01L_selector_data(disableLooping)" = private global [15 x i8] c"disableLooping\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(disableLooping)" = private externally_initialized global ptr @"\01L_selector_data(disableLooping)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@"$sytN" = external global %swift.full_existential_type
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAMc" = constant %swift.protocol_conformance_descriptor { i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib22FkADqqqqqqqqqqqqqqqqqqMp" to i64), i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAMc" to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (%swift.protocol_conformance_descriptor, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAMc", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAWP" to i64), i64 ptrtoint (ptr getelementptr inbounds (%swift.protocol_conformance_descriptor, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAMc", i32 0, i32 2) to i64)) to i32), i32 0 }, section "__TEXT,__const", no_sanitize_address, align 4
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4play9timeRangeySo06CMTimeI0a_tYaFTWTu" = linkonce_odr hidden global %swift.async_func_pointer <{ i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4play9timeRangeySo06CMTimeI0a_tYaFTW" to i64), i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4play9timeRangeySo06CMTimeI0a_tYaFTWTu" to i64)) to i32), i32 32 }>, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAWP" = constant [23 x ptr] [ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAMc", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvsTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvMTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP11playerLayerSo7CALayerCvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avsTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avMTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvsTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvMTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP6statusAA0C14PlaybackStatusOvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvsTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvMTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP16debugDescriptionSSvgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP11currentTimeSo6CMTimeavgTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4playyyFTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4play9timeRangeySo06CMTimeI0a_tYaFTWTu", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP5pauseyyFTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4seek2toySo6CMTimea_tFTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP16addDisplayEffectyyyXlFTW", ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP19removeDisplayEffectyyyXlFTW"], align 8
@.str.16.ReproKvoVideoLib = private constant [17 x i8] c"ReproKvoVideoLib\00"
@"$s16ReproKvoVideoLibMXM" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.16.ReproKvoVideoLib to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLibMXM", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__constg_swiftt", align 4
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11videoPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo07AVQueueG0CvpWvd" = internal constant i64 16, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC34$__lazy_storage_$_videoPlayerLayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0dJ0CSgvpWvd" = internal constant i64 24, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC6looper33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0D6LooperCSgvpWvd" = internal constant i64 32, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvpWvd" = internal constant i64 40, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC20boundaryTimeObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLLAA0c14PlayerBoundaryH0VSgvpWvd" = internal constant i64 48, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC8delegateAA0C14PlayerDelegate_pSgvpWvd" = constant i64 104, align 8
@"$sBoWV" = external global ptr, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMm" = global %objc_class { ptr @"OBJC_FkATqqqqqq$__TtCs12_SwiftObject", ptr @"OBJC_FkATqqqqqq$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 ptrtoint (ptr @_FkAGqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq to i64) }, align 8
@"OBJC_CLASS_$__TtCs12_SwiftObject" = external global %objc_class, align 8
@_objc_empty_cache = external global %swift.opaque
@"OBJC_FkATqqqqqq$__TtCs12_SwiftObject" = external global %objc_class, align 8
@.str.48._TtC16ReproKvoVideoLib24ReproVideoPlayerProvider = private unnamed_addr constant [49 x i8] c"_TtC16ReproKvoVideoLib24ReproVideoPlayerProvider\00", section "__TEXT,__objc_classname,cstring_literals"
@_FkAGqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 129, i32 40, i32 40, i32 0, ptr null, ptr @.str.48._TtC16ReproKvoVideoLib24ReproVideoPlayerProvider, ptr null, ptr null, ptr null, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.11.videoPlayer = private unnamed_addr constant [12 x i8] c"videoPlayer\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.0. = private unnamed_addr constant [1 x i8] zeroinitializer, section "__TEXT,__objc_methtype,cstring_literals"
@".str.34.$__lazy_storage_$_videoPlayerLayer" = private unnamed_addr constant [35 x i8] c"$__lazy_storage_$_videoPlayerLayer\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.6.looper = private unnamed_addr constant [7 x i8] c"looper\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.14.statusObserver = private unnamed_addr constant [15 x i8] c"statusObserver\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.20.boundaryTimeObserver = private unnamed_addr constant [21 x i8] c"boundaryTimeObserver\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.8.delegate = private unnamed_addr constant [9 x i8] c"delegate\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.12.playbackItem.5 = private unnamed_addr constant [13 x i8] c"playbackItem\00", section "__TEXT,__objc_methname,cstring_literals"
@_IVARS__TtC16ReproKvoVideoLib24ReproVideoPlayerProvider = internal constant { i32, i32, [7 x { ptr, ptr, ptr, i32, i32 }] } { i32 32, i32 7, [7 x { ptr, ptr, ptr, i32, i32 }] [{ ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11videoPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo07AVQueueG0CvpWvd", ptr @.str.11.videoPlayer, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC34$__lazy_storage_$_videoPlayerLayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0dJ0CSgvpWvd", ptr @".str.34.$__lazy_storage_$_videoPlayerLayer", ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC6looper33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0D6LooperCSgvpWvd", ptr @.str.6.looper, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvpWvd", ptr @.str.14.statusObserver, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC20boundaryTimeObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLLAA0c14PlayerBoundaryH0VSgvpWvd", ptr @.str.20.boundaryTimeObserver, ptr @.str.0., i32 3, i32 56 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC8delegateAA0C14PlayerDelegate_pSgvpWvd", ptr @.str.8.delegate, ptr @.str.0., i32 3, i32 16 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC12playbackItemAA0c8PlaybackG0OSgvpWvd", ptr @.str.12.playbackItem.5, ptr @.str.0., i32 0, i32 0 }] }, section "__DATA, __objc_const", align 8
@_DATA__TtC16ReproKvoVideoLib24ReproVideoPlayerProvider = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 192, i32 16, i32 120, i32 0, ptr null, ptr @.str.48._TtC16ReproKvoVideoLib24ReproVideoPlayerProvider, ptr null, ptr null, ptr @_IVARS__TtC16ReproKvoVideoLib24ReproVideoPlayerProvider, ptr null, ptr null, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMU" }, section "__DATA, __objc_data", align 8
@.str.24.ReproVideoPlayerProvider = private constant [25 x i8] c"ReproVideoPlayerProvider\00"
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMl" = internal global { ptr, ptr } zeroinitializer, align 8
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn" = constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }> <{ i32 -2147418032, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLibMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.24.ReproVideoPlayerProvider to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 4) to i64)) to i32), i32 0, i32 3, i32 18, i32 8, i32 7, i32 10, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMl" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 11) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, i64, i64, i64, i64, i64, ptr }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMf", i32 0, i32 3) to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 12) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMr" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 13) to i64)) to i32), i32 17, i32 1, %swift.method_descriptor { i32 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCACycfC" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", i32 0, i32 16, i32 1) to i64)) to i32) } }>, section "__TEXT,__constg_swiftt", align 4
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMf" = internal global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, i64, i64, i64, i64, i64, ptr }> <{ ptr null, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCfD", ptr @"$sBoWV", i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMm" to i64), ptr @"OBJC_CLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 add (i64 ptrtoint (ptr @_DATA__TtC16ReproKvoVideoLib24ReproVideoPlayerProvider to i64), i64 2), i32 2, i32 0, i32 0, i16 0, i16 0, i32 168, i32 24, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn", ptr null, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCACycfC" }>, align 8
@"symbolic _____ 16ReproKvoVideoLib24ReproVideoPlayerProviderC" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 16ReproKvoVideoLib24ReproVideoPlayerProviderC", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic So13AVQueuePlayerC" = linkonce_odr hidden constant <{ [18 x i8], i8 }> <{ [18 x i8] c"So13AVQueuePlayerC", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@0 = private constant [12 x i8] c"videoPlayer\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic So13AVPlayerLayerCSg" = linkonce_odr hidden constant <{ [20 x i8], i8 }> <{ [20 x i8] c"So13AVPlayerLayerCSg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@1 = private constant [35 x i8] c"$__lazy_storage_$_videoPlayerLayer\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic So14AVPlayerLooperCSg" = linkonce_odr hidden constant <{ [21 x i8], i8 }> <{ [21 x i8] c"So14AVPlayerLooperCSg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@2 = private constant [7 x i8] c"looper\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s10Foundation21NSKeyValueObservationCMn" = external global %swift.type_descriptor, align 4
@"got.$s10Foundation21NSKeyValueObservationCMn" = private unnamed_addr constant ptr @"$s10Foundation21NSKeyValueObservationCMn"
@"symbolic _____Sg 10Foundation21NSKeyValueObservationC" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s10Foundation21NSKeyValueObservationCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg 10Foundation21NSKeyValueObservationC", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@3 = private constant [15 x i8] c"statusObserver\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic _____Sg 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@4 = private constant [21 x i8] c"boundaryTimeObserver\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic ______pSgXw 16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP" = linkonce_odr hidden constant <{ i8, i32, [6 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqMp" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [6 x i8], i8 }>, ptr @"symbolic ______pSgXw 16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP", i32 0, i32 1) to i64)) to i32), [6 x i8] c"_pSgXw", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@5 = private constant [9 x i8] c"delegate\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@6 = private constant [13 x i8] c"playbackItem\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16ReproKvoVideoLib24ReproVideoPlayerProviderC" to i64), i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF" to i64)) to i32), i32 0, i16 1, i16 12, i32 7, i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic So13AVQueuePlayerC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @0 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 7) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic So13AVPlayerLayerCSg" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @1 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 10) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic So14AVPlayerLooperCSg" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 12) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @2 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 13) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 10Foundation21NSKeyValueObservationC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 15) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @3 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 16) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 18) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @4 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 19) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic ______pSgXw 16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 21) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @5 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 22) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 24) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @6 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMF", i32 0, i32 25) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOWV" = internal global %swift.enum_vwtable { ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwCP", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwxx", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwcp", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwca", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwtk", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwta", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwet", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwst", i64 0, i64 0, i32 6291456, i32 0, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwug", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwup", ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwui" }, align 8
@.str.20.FkACqqqqqqqqqqqqqqqq = private constant [21 x i8] c"FkACqqqqqqqqqqqqqqqq\00"
@"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMl" = internal global { ptr, ptr } zeroinitializer, align 8
@"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn" = constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 65618, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLibMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.20.FkACqqqqqqqqqqqqqqqq to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 4) to i64)) to i32), i32 33554434, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMl" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 7) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr getelementptr inbounds (<{ ptr, ptr, i64, ptr, i64 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMf", i32 0, i32 2) to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 8) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMr" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i32 0, i32 9) to i64)) to i32) }>, section "__TEXT,__constg_swiftt", align 4
@"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMf" = internal global <{ ptr, ptr, i64, ptr, i64 }> <{ ptr null, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOWV", i64 513, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn", i64 0 }>, align 8
@"symbolic _____ 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@7 = private constant [4 x i8] c"url\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@8 = private constant [13 x i8] c"dashManifest\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO" to i64), i64 ptrtoint (ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF" to i64)) to i32), i32 0, i16 2, i16 12, i32 2, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic ______AASg5hdUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @7 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF", i32 0, i32 7) to i64)) to i32), i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic SS8manifest______Sg11fallbackUrlSb9isLoopingSS16stringIdentifiert 10Foundation3URLV" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @8 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMF", i32 0, i32 10) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@.str.30.FkAAqqqqqqqqqqqqqqqqqqqqqqqqqq = private constant [31 x i8] c"FkAAqqqqqqqqqqqqqqqqqqqqqqqqqq\00"
@"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn" = constant <{ i32, i32, i32, i32, i32, i32, i32 }> <{ i32 81, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLibMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.30.FkAAqqqqqqqqqqqqqqqqqqqqqqqqqq to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn", i32 0, i32 4) to i64)) to i32), i32 2, i32 2 }>, section "__TEXT,__constg_swiftt", align 4
@"symbolic _____ 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic _____ So6CMTimea" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo6CMTimeaMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ So6CMTimea", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@18 = private constant [8 x i8] c"endTime\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic yp" = linkonce_odr hidden constant <{ [2 x i8], i8 }> <{ [2 x i8] c"yp", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@19 = private constant [14 x i8] c"observerToken\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqV" to i64), i64 ptrtoint (ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF" to i64)) to i32), i32 0, i16 0, i16 12, i32 2, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ So6CMTimea" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @18 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF", i32 0, i32 7) to i64)) to i32), i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic yp" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @19 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMF", i32 0, i32 10) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@.str.22.FkABqqqqqqqqqqqqqqqqqq = private constant [23 x i8] c"FkABqqqqqqqqqqqqqqqqqq\00"
@"symbolic x" = linkonce_odr hidden constant <{ [1 x i8], i8 }> <{ [1 x i8] c"x", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqMp" = constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }> <{ i32 67, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLibMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqMp", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.22.FkABqqqqqqqqqqqqqqqqqq to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqMp", i32 0, i32 2) to i64)) to i32), i32 1, i32 5, i32 0, i32 31, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic x" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqMp", i32 0, i32 7) to i64)) to i32), i32 0, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 } }>, section "__TEXT,__constg_swiftt", align 4
@.str.22.FkADqqqqqqqqqqqqqqqqqq = private constant [23 x i8] c"FkADqqqqqqqqqqqqqqqqqq\00"
@"$s16ReproKvoVideoLib22FkADqqqqqqqqqqqqqqqqqqMp" = constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }> <{ i32 67, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLibMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkADqqqqqqqqqqqqqqqqqqMp", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.22.FkADqqqqqqqqqqqqqqqqqq to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkADqqqqqqqqqqqqqqqqqqMp", i32 0, i32 2) to i64)) to i32), i32 1, i32 22, i32 0, i32 31, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic x" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement, %swift.protocol_requirement }>, ptr @"$s16ReproKvoVideoLib22FkADqqqqqqqqqqqqqqqqqqMp", i32 0, i32 7) to i64)) to i32), i32 0, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 20, i32 0 }, %swift.protocol_requirement { i32 22, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 20, i32 0 }, %swift.protocol_requirement { i32 22, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 20, i32 0 }, %swift.protocol_requirement { i32 22, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 20, i32 0 }, %swift.protocol_requirement { i32 22, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 19, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 49, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 }, %swift.protocol_requirement { i32 17, i32 0 } }>, section "__TEXT,__constg_swiftt", align 4
@.str.3.__C = private constant [4 x i8] c"__C\00"
@"$sSoMXM" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.3.__C to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32 }>, ptr @"$sSoMXM", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__constg_swiftt", align 4
@.str.10.CMTime_St_.nul14.nul17 = private constant [11 x i8] c"CMTime\00St\00\00"
@"$sSo6CMTimeaMn" = linkonce_odr hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 393233, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSoMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo6CMTimeaMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.10.CMTime_St_.nul14.nul17 to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo6CMTimeaMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo6CMTimeaMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo6CMTimeaMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo6CMTimeaMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo6CMTimeaMn", i32 0, i32 4) to i64)) to i32), i32 4, i32 2, i32 0 }>, section "__TEXT,__constg_swiftt", align 4
@.str.29.Status_NAVPlayerLooperStatus_.nul14.nul36 = private constant [30 x i8] c"Status\00NAVPlayerLooperStatus\00\00"
@"$sSo20AVPlayerLooperStatusVMn" = linkonce_odr hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 393234, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSoMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo20AVPlayerLooperStatusVMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.29.Status_NAVPlayerLooperStatus_.nul14.nul36 to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo20AVPlayerLooperStatusVMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo20AVPlayerLooperStatusVMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo20AVPlayerLooperStatusVMn", i32 0, i32 3) to i64)) to i32), i32 0, i32 0, i32 4, i32 0 }>, section "__TEXT,__constg_swiftt", align 4
@22 = private constant [9 x i8] c"rawValue\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$ss5Int64VMn" = external global %swift.type_descriptor, align 4
@"got.$ss5Int64VMn" = private unnamed_addr constant ptr @"$ss5Int64VMn"
@"symbolic _____ s5Int64V" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5Int64VMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ s5Int64V", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@24 = private constant [6 x i8] c"value\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$ss5Int32VMn" = external global %swift.type_descriptor, align 4
@"got.$ss5Int32VMn" = private unnamed_addr constant ptr @"$ss5Int32VMn"
@"symbolic _____ s5Int32V" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5Int32VMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ s5Int32V", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@25 = private constant [10 x i8] c"timescale\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic _____ So11CMTimeFlagsV" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo11CMTimeFlagsVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ So11CMTimeFlagsV", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@26 = private constant [6 x i8] c"flags\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@27 = private constant [6 x i8] c"epoch\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$sSo6CMTimeaMF" = linkonce_odr hidden constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ So6CMTimea" to i64), i64 ptrtoint (ptr @"$sSo6CMTimeaMF" to i64)) to i32), i32 0, i16 0, i16 12, i32 4, i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ s5Int64V" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @24 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 7) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ s5Int32V" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @25 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 10) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ So11CMTimeFlagsV" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 12) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @26 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 13) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ s5Int64V" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 15) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @27 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$sSo6CMTimeaMF", i32 0, i32 16) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@"symbolic _____SgXw 16ReproKvoVideoLib24ReproVideoPlayerProviderC" = linkonce_odr hidden constant <{ i8, i32, [4 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____SgXw 16ReproKvoVideoLib24ReproVideoPlayerProviderC", i32 0, i32 1) to i64)) to i32), [4 x i8] c"SgXw", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"\01l__swift5_reflection_descriptor.18" = private constant { i32, i32, i32, i32 } { i32 1, i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____SgXw 16ReproKvoVideoLib24ReproVideoPlayerProviderC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32 }, ptr @"\01l__swift5_reflection_descriptor.18", i32 0, i32 3) to i64)) to i32) }, section "__TEXT,__swift5_capture, regular", no_sanitize_address, align 4
@metadata.20 = private constant %swift.full_boxmetadata { ptr @objectdestroy.19, ptr null, %swift.type { i64 1024 }, i32 16, ptr @"\01l__swift5_reflection_descriptor.18" }, align 8
@".str.47.ReproKvoVideoLib/ReproVideoPlayerProvider.swift" = private unnamed_addr constant [48 x i8] c"ReproKvoVideoLib/ReproVideoPlayerProvider.swift\00"
@"\01L_selector_data(error)" = private global [6 x i8] c"error\00", section "__TEXT,__objc_methname,cstring_literals", align 1
@"\01L_selector(error)" = private externally_initialized global ptr @"\01L_selector_data(error)", section "__DATA,__objc_selrefs,literal_pointers,no_dead_strip", align 8
@".str.42.Video playback item must have a valid URL." = private unnamed_addr constant [43 x i8] c"Video playback item must have a valid URL.\00"
@.str.11.CMTimeFlags = private constant [12 x i8] c"CMTimeFlags\00"
@"$sSo11CMTimeFlagsVMn" = linkonce_odr hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 131089, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSoMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo11CMTimeFlagsVMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.11.CMTimeFlags to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo11CMTimeFlagsVMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo11CMTimeFlagsVMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo11CMTimeFlagsVMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$sSo11CMTimeFlagsVMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32 }>, ptr @"$sSo11CMTimeFlagsVMn", i32 0, i32 4) to i64)) to i32), i32 1, i32 2, i32 0 }>, section "__TEXT,__constg_swiftt", align 4
@"$ss6UInt32VMn" = external global %swift.type_descriptor, align 4
@"got.$ss6UInt32VMn" = private unnamed_addr constant ptr @"$ss6UInt32VMn"
@"symbolic _____ s6UInt32V" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss6UInt32VMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ s6UInt32V", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$sSo11CMTimeFlagsVMF" = linkonce_odr hidden constant { i32, i32, i16, i16, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ So11CMTimeFlagsV" to i64), i64 ptrtoint (ptr @"$sSo11CMTimeFlagsVMF" to i64)) to i32), i32 0, i16 0, i16 12, i32 1, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ s6UInt32V" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32 }, ptr @"$sSo11CMTimeFlagsVMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @22 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32 }, ptr @"$sSo11CMTimeFlagsVMF", i32 0, i32 7) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
declare hidden swiftcc %swift.metadata_response @"$sSo13AVQueuePlayerCMa"(i64) #1
declare hidden swiftcc ptr @"$sSo13AVQueuePlayerCABycfC"(ptr swiftself) #0
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #4
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #2
declare ptr @llvm.objc.retain(ptr returned) #2
declare void @swift_endAccess(ptr) #2
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #4
declare void @llvm.objc.release(ptr) #2
declare swiftcc ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC16videoPlayerLayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0dH0Cvg"(ptr swiftself) #5
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #6
define swiftcc i64 @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvpfi"() #0 !dbg !283 {
  ret i64 0, !dbg !287
}
define internal swiftcc i64 @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvg"(ptr swiftself %0) #0 !dbg !288 {
  %2 = alloca [24 x i8], align 8
  %3 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %0, i32 0, i32 4, !dbg !292
  call void @llvm.lifetime.start.p0(i64 -1, ptr %2), !dbg !292
  call void @swift_beginAccess(ptr %3, ptr %2, i64 32, ptr null) #2, !dbg !292
  %4 = load i64, ptr %3, align 8, !dbg !292
  %5 = inttoptr i64 %4 to ptr, !dbg !292
  %6 = call ptr @llvm.objc.retain(ptr %5), !dbg !292
  call void @swift_endAccess(ptr %2) #2, !dbg !292
  call void @llvm.lifetime.end.p0(i64 -1, ptr %2), !dbg !292
  ret i64 %4, !dbg !292
}
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvs"(i64 %0, ptr swiftself %1) #0 !dbg !293 {
  %3 = alloca [24 x i8], align 8
  %4 = inttoptr i64 %0 to ptr, !dbg !297
  %5 = call ptr @llvm.objc.retain(ptr %4), !dbg !297
  %6 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 4, !dbg !297
  call void @llvm.lifetime.start.p0(i64 -1, ptr %3), !dbg !297
  call void @swift_beginAccess(ptr %6, ptr %3, i64 33, ptr null) #2, !dbg !297
  %7 = load i64, ptr %6, align 8, !dbg !297
  store i64 %0, ptr %6, align 8, !dbg !297
  %8 = inttoptr i64 %7 to ptr, !dbg !297
  call void @llvm.objc.release(ptr %8), !dbg !297
  call void @swift_endAccess(ptr %3) #2, !dbg !297
  call void @llvm.lifetime.end.p0(i64 -1, ptr %3), !dbg !297
  %9 = inttoptr i64 %0 to ptr, !dbg !297
  call void @llvm.objc.release(ptr %9), !dbg !297
  ret void, !dbg !297
}
declare ptr @swift_retain(ptr returned) #9
declare void @swift_release(ptr) #2
declare swiftcc ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCACycfC"(ptr swiftself) #0
declare ptr @swift_allocObject(ptr, i64, i64) #2
declare swiftcc %swift.metadata_response @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMa"(i64) #1
declare hidden ptr @"$sSo14AVPlayerLooperCSgWOh"(ptr) #7
declare swiftcc %swift.metadata_response @"$s10Foundation3URLVMa"(i64) #0
declare hidden ptr @__swift_instantiateConcreteTypeFromMangledName(ptr) #11
declare void @objc_msgSend()
declare void @swift_unknownObjectRelease(ptr) #2
declare swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCfD"(ptr swiftself) #0
declare swiftcc { i64, ptr } @"$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC"(ptr, i64, i1) #0
declare ptr @swift_unknownObjectWeakLoadStrong(ptr) #9
declare ptr @swift_unknownObjectRetain(ptr returned) #2
declare hidden ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqq_pSgWOh"(ptr) #7
declare ptr @llvm.objc.retainAutoreleasedReturnValue(ptr) #2
declare hidden swiftcc i1 @"$s13FkAEqqqqqqqqq0A7MustFix_10allThreads4file4line8functionySSyXK_Sbs12StaticStringVSuAHtFfA0_"() #0
declare swiftcc void @"$s13FkAEqqqqqqqqq0A7MustFix_10allThreads4file4line8functionySSyXK_Sbs12StaticStringVSuAHtF"(ptr, ptr, i1, i64, i64, i8, i64, i64, i64, i8) #0
declare swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14tearDownPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLyyF"(ptr swiftself) #0
declare ptr @swift_getObjectType(ptr) #10
declare hidden ptr @"$s10Foundation3URLVSgWOh"(ptr) #7
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tF"(ptr noalias %0, ptr swiftself %1) #0 !dbg !667 {
  %3 = alloca ptr, align 8
    #dbg_declare(ptr %3, !672, !DIExpression(DW_OP_deref), !686)
  call void @llvm.memset.p0.i64(ptr align 8 %3, i8 0, i64 8, i1 false)
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !676, !DIExpression(DW_OP_deref), !687)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  %5 = alloca ptr, align 8
    #dbg_declare(ptr %5, !677, !DIExpression(), !688)
  call void @llvm.memset.p0.i64(ptr align 8 %5, i8 0, i64 8, i1 false)
  %6 = alloca [24 x i8], align 8
  %7 = alloca i1, align 8
    #dbg_declare(ptr %7, !678, !DIExpression(), !689)
  call void @llvm.memset.p0.i64(ptr align 8 %7, i8 0, i64 1, i1 false)
  %8 = alloca [24 x i8], align 8
  %9 = alloca <{ %TSo14AVPlayerLooperCSg, %TSo14AVPlayerLooperCSg }>, align 8
  %10 = alloca [24 x i8], align 8
  %11 = alloca [24 x i8], align 8
  %12 = alloca [24 x i8], align 8
  %13 = alloca ptr, align 8
    #dbg_declare(ptr %13, !680, !DIExpression(), !690)
  call void @llvm.memset.p0.i64(ptr align 8 %13, i8 0, i64 8, i1 false)
  %14 = alloca [24 x i8], align 8
  %15 = alloca [24 x i8], align 8
  %16 = alloca [24 x i8], align 8
  %17 = alloca [24 x i8], align 8
  %18 = alloca ptr, align 8
  %19 = alloca %TSo26NSKeyValueObservingOptionsV, align 8
  %20 = alloca [24 x i8], align 8
  %21 = alloca [24 x i8], align 8
  %22 = alloca %T16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqq_pSg, align 8
  %23 = alloca %TSo14AVPlayerLooperCSg, align 8
  %24 = alloca ptr, align 8
  %25 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s10Foundation3URLVSgMD") #15, !dbg !691
  %26 = getelementptr inbounds ptr, ptr %25, i64 -1, !dbg !691
  %27 = load ptr, ptr %26, align 8, !dbg !691, !invariant.load !310, !dereferenceable !311
  %28 = getelementptr inbounds nuw %swift.vwtable, ptr %27, i32 0, i32 8, !dbg !691
  %29 = load i64, ptr %28, align 8, !dbg !691, !invariant.load !310
  %30 = alloca i8, i64 %29, align 16, !dbg !691
  call void @llvm.lifetime.start.p0(i64 -1, ptr %30), !dbg !691
  %31 = call swiftcc %swift.metadata_response @"$s10Foundation3URLVMa"(i64 0) #19, !dbg !694
  %32 = extractvalue %swift.metadata_response %31, 0, !dbg !694
  %33 = getelementptr inbounds ptr, ptr %32, i64 -1, !dbg !694
  %34 = load ptr, ptr %33, align 8, !dbg !694, !invariant.load !310, !dereferenceable !311
  %35 = getelementptr inbounds nuw %swift.vwtable, ptr %34, i32 0, i32 8, !dbg !694
  %36 = load i64, ptr %35, align 8, !dbg !694, !invariant.load !310
  %37 = alloca i8, i64 %36, align 16, !dbg !694
  call void @llvm.lifetime.start.p0(i64 -1, ptr %37), !dbg !694
  %38 = alloca i8, i64 %36, align 16, !dbg !697
  call void @llvm.lifetime.start.p0(i64 -1, ptr %38), !dbg !697
  store ptr %38, ptr %3, align 8, !dbg !699
  store ptr %0, ptr %4, align 8, !dbg !700
  store ptr %1, ptr %5, align 8, !dbg !700
  call swiftcc void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO3url10Foundation3URLVSgvg"(ptr noalias sret(%swift.opaque) %30, ptr noalias swiftself %0), !dbg !701
  %39 = getelementptr inbounds ptr, ptr %34, i32 6, !dbg !701
  %40 = load ptr, ptr %39, align 8, !dbg !701, !invariant.load !310
  %41 = call i32 %40(ptr noalias %30, i32 1, ptr %32) #15, !dbg !701
  %42 = icmp ne i32 %41, 1
  br i1 %42, label %44, label %208

43:                                               ; No predecessors!
  unreachable, !dbg !701

44:                                               ; preds = %2
  %45 = getelementptr inbounds ptr, ptr %34, i32 4, !dbg !701
  %46 = load ptr, ptr %45, align 8, !dbg !701, !invariant.load !310
  %47 = call ptr %46(ptr noalias %38, ptr noalias %30, ptr %32) #2, !dbg !701
  %48 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 1, !dbg !702
  call void @llvm.lifetime.start.p0(i64 -1, ptr %6), !dbg !702
  call void @swift_beginAccess(ptr %48, ptr %6, i64 32, ptr null) #2, !dbg !702
  %49 = load ptr, ptr %48, align 8, !dbg !702
  %50 = call ptr @llvm.objc.retain(ptr %49), !dbg !702
  call void @swift_endAccess(ptr %6) #2, !dbg !704
  call void @llvm.lifetime.end.p0(i64 -1, ptr %6), !dbg !704
  %51 = load ptr, ptr @"\01L_selector(isMuted)", align 8, !dbg !705
  %52 = call zeroext i1 @objc_msgSend(ptr %49, ptr %51), !dbg !705
  call void @llvm.objc.release(ptr %49), !dbg !706
  store i1 %52, ptr %7, align 8, !dbg !708
  %53 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 3, !dbg !709
  call void @llvm.lifetime.start.p0(i64 -1, ptr %8), !dbg !709
  call void @swift_beginAccess(ptr %53, ptr %8, i64 32, ptr null) #2, !dbg !709
  %54 = load i64, ptr %53, align 8, !dbg !709
  %55 = inttoptr i64 %54 to ptr, !dbg !709
  %56 = call ptr @llvm.objc.retain(ptr %55), !dbg !709
  call void @swift_endAccess(ptr %8) #2, !dbg !711
  call void @llvm.lifetime.end.p0(i64 -1, ptr %8), !dbg !711
  call void @llvm.lifetime.start.p0(i64 16, ptr %9), !dbg !712
  %57 = getelementptr inbounds nuw <{ %TSo14AVPlayerLooperCSg, %TSo14AVPlayerLooperCSg }>, ptr %9, i32 0, i32 0, !dbg !714
  %58 = getelementptr inbounds nuw <{ %TSo14AVPlayerLooperCSg, %TSo14AVPlayerLooperCSg }>, ptr %9, i32 0, i32 1, !dbg !714
  store i64 %54, ptr %57, align 8, !dbg !714
  store i64 0, ptr %58, align 8, !dbg !714
  %59 = load i64, ptr %57, align 8, !dbg !714
  %60 = icmp eq i64 %59, 0, !dbg !714
  br i1 %60, label %65, label %61, !dbg !714

61:                                               ; preds = %44
  call void @llvm.lifetime.start.p0(i64 8, ptr %23), !dbg !712
  %62 = call ptr @"$sSo14AVPlayerLooperCSgWOc"(ptr %57, ptr %23), !dbg !714
  %63 = load i64, ptr %58, align 8, !dbg !714
  %64 = icmp eq i64 %63, 0, !dbg !714
  br i1 %64, label %78, label %68, !dbg !714

65:                                               ; preds = %44
  %66 = load i64, ptr %58, align 8, !dbg !714
  %67 = icmp eq i64 %66, 0, !dbg !714
  br i1 %67, label %85, label %87, !dbg !714

68:                                               ; preds = %61
  call void @llvm.lifetime.start.p0(i64 8, ptr %24), !dbg !712
  %69 = load ptr, ptr %58, align 8, !dbg !714
  store ptr %69, ptr %24, align 8, !dbg !714
  %70 = call swiftcc %swift.metadata_response @"$sSo14AVPlayerLooperCMa"(i64 0) #19, !dbg !714
  %71 = extractvalue %swift.metadata_response %70, 0, !dbg !714
  %72 = load ptr, ptr %23, align 8, !dbg !714
  %73 = load ptr, ptr %24, align 8, !dbg !714
  %74 = call swiftcc i1 @"$sSo8NSObjectC10ObjectiveCE2eeoiySbAB_ABtFZ"(ptr %72, ptr %73, ptr swiftself %71), !dbg !714
  %75 = load ptr, ptr %24, align 8, !dbg !714
  call void @llvm.objc.release(ptr %75), !dbg !714
  %76 = load ptr, ptr %23, align 8, !dbg !714
  call void @llvm.objc.release(ptr %76), !dbg !714
  call void @llvm.lifetime.end.p0(i64 8, ptr %24), !dbg !714
  call void @llvm.lifetime.end.p0(i64 8, ptr %23), !dbg !714
  %77 = call ptr @"$sSo14AVPlayerLooperCSgWOh"(ptr %57), !dbg !714
  br label %80, !dbg !714

78:                                               ; preds = %61
  %79 = load ptr, ptr %23, align 8, !dbg !714
  call void @llvm.objc.release(ptr %79), !dbg !714
  call void @llvm.lifetime.end.p0(i64 8, ptr %23), !dbg !714
  br label %83, !dbg !714

80:                                               ; preds = %85, %83, %68
  %81 = phi i1 [ false, %83 ], [ true, %85 ], [ %74, %68 ], !dbg !714
  call void @llvm.lifetime.end.p0(i64 16, ptr %9), !dbg !714
  %82 = xor i1 %81, true, !dbg !714
  br i1 %82, label %88, label %119, !dbg !714

83:                                               ; preds = %87, %78
  %84 = call ptr @"$sSo14AVPlayerLooperCSg_ACtWOh"(ptr %9), !dbg !714
  br label %80, !dbg !714

85:                                               ; preds = %65
  %86 = call ptr @"$sSo14AVPlayerLooperCSgWOh"(ptr %57), !dbg !714
  br label %80, !dbg !714

87:                                               ; preds = %65
  br label %83, !dbg !714

88:                                               ; preds = %80
  call swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14tearDownPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLyyF"(ptr swiftself %1), !dbg !715
  %89 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 6, !dbg !717
  call void @llvm.lifetime.start.p0(i64 -1, ptr %21), !dbg !717
  call void @swift_beginAccess(ptr %89, ptr %21, i64 32, ptr null) #2, !dbg !717
  call void @llvm.lifetime.start.p0(i64 16, ptr %22), !dbg !718
  %90 = getelementptr inbounds nuw { %swift.weak, ptr }, ptr %89, i32 0, i32 0, !dbg !717
  %91 = call ptr @swift_unknownObjectWeakLoadStrong(ptr %90) #2, !dbg !717
  %92 = getelementptr inbounds nuw { %swift.weak, ptr }, ptr %89, i32 0, i32 1, !dbg !717
  %93 = load ptr, ptr %92, align 8, !dbg !717
  %94 = ptrtoint ptr %91 to i64, !dbg !717
  %95 = ptrtoint ptr %93 to i64, !dbg !717
  %96 = getelementptr inbounds nuw { i64, i64 }, ptr %22, i32 0, i32 0, !dbg !717
  store i64 %94, ptr %96, align 8, !dbg !717
  %97 = getelementptr inbounds nuw { i64, i64 }, ptr %22, i32 0, i32 1, !dbg !717
  store i64 %95, ptr %97, align 8, !dbg !717
  %98 = getelementptr inbounds nuw { i64, i64 }, ptr %22, i32 0, i32 0, !dbg !717
  %99 = load i64, ptr %98, align 8, !dbg !717
  %100 = getelementptr inbounds nuw { i64, i64 }, ptr %22, i32 0, i32 1, !dbg !717
  %101 = load i64, ptr %100, align 8, !dbg !717
  %102 = icmp eq i64 %99, 0, !dbg !717
  %103 = xor i1 %102, true, !dbg !717
  br i1 %103, label %104, label %117, !dbg !717

104:                                              ; preds = %88
  %105 = getelementptr inbounds nuw %T16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP, ptr %22, i32 0, i32 0, !dbg !717
  %106 = load ptr, ptr %105, align 8, !dbg !717
  %107 = getelementptr inbounds nuw %T16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqqP, ptr %22, i32 0, i32 1, !dbg !717
  %108 = load ptr, ptr %107, align 8, !dbg !717
  %109 = call ptr @swift_unknownObjectRetain(ptr returned %106) #2, !dbg !717
  %110 = call ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqq_pSgWOh"(ptr %22), !dbg !720
  call void @swift_endAccess(ptr %21) #2, !dbg !722
  call void @llvm.lifetime.end.p0(i64 -1, ptr %21), !dbg !722
  %111 = call ptr @swift_getObjectType(ptr %106) #10, !dbg !722
  %112 = call ptr @swift_retain(ptr returned %1) #9, !dbg !723
  %113 = getelementptr inbounds ptr, ptr %108, i32 3, !dbg !724
  %114 = load ptr, ptr %113, align 8, !dbg !724, !invariant.load !310
  call swiftcc void %114(ptr %1, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0AAWP", ptr swiftself %106, ptr %111, ptr %108), !dbg !724
  call void @swift_unknownObjectRelease(ptr %1) #2, !dbg !720
  call void @swift_unknownObjectRelease(ptr %106) #2, !dbg !720
  call void @llvm.lifetime.end.p0(i64 16, ptr %22), !dbg !722
  br label %115, !dbg !722

115:                                              ; preds = %117, %104
  %116 = phi i8 [ 1, %117 ], [ 0, %104 ], !dbg !725
  br label %120, !dbg !725

117:                                              ; preds = %88
  %118 = call ptr @"$s16ReproKvoVideoLib22FkABqqqqqqqqqqqqqqqqqq_pSgWOh"(ptr %22), !dbg !720
  call void @llvm.lifetime.end.p0(i64 16, ptr %22), !dbg !720
  call void @swift_endAccess(ptr %21) #2, !dbg !722
  call void @llvm.lifetime.end.p0(i64 -1, ptr %21), !dbg !722
  br label %115, !dbg !722

119:                                              ; preds = %80
  br label %120, !dbg !726

120:                                              ; preds = %119, %115
  %121 = call swiftcc %swift.metadata_response @"$sSo13AVQueuePlayerCMa"(i64 0) #19, !dbg !727
  %122 = extractvalue %swift.metadata_response %121, 0, !dbg !727
  %123 = call swiftcc ptr @"$sSo13AVQueuePlayerCABycfC"(ptr swiftself %122), !dbg !727
  %124 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 1, !dbg !728
  call void @llvm.lifetime.start.p0(i64 -1, ptr %10), !dbg !728
  call void @swift_beginAccess(ptr %124, ptr %10, i64 33, ptr null) #2, !dbg !728
  %125 = load ptr, ptr %124, align 8, !dbg !728
  store ptr %123, ptr %124, align 8, !dbg !728
  call void @llvm.objc.release(ptr %125), !dbg !728
  call void @swift_endAccess(ptr %10) #2, !dbg !708
  call void @llvm.lifetime.end.p0(i64 -1, ptr %10), !dbg !708
  %126 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 1, !dbg !729
  call void @llvm.lifetime.start.p0(i64 -1, ptr %11), !dbg !729
  call void @swift_beginAccess(ptr %126, ptr %11, i64 32, ptr null) #2, !dbg !729
  %127 = load ptr, ptr %126, align 8, !dbg !729
  %128 = call ptr @llvm.objc.retain(ptr %127), !dbg !729
  call void @swift_endAccess(ptr %11) #2, !dbg !708
  call void @llvm.lifetime.end.p0(i64 -1, ptr %11), !dbg !708
  %129 = load ptr, ptr @"\01L_selector(setMuted:)", align 8, !dbg !730
  call void @objc_msgSend(ptr %127, ptr %129, i1 zeroext %52), !dbg !730
  call void @llvm.objc.release(ptr %127), !dbg !731
  %130 = call swiftcc ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC16videoPlayerLayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLLSo0dH0Cvg"(ptr swiftself %1), !dbg !733
  %131 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 1, !dbg !734
  call void @llvm.lifetime.start.p0(i64 -1, ptr %12), !dbg !734
  call void @swift_beginAccess(ptr %131, ptr %12, i64 32, ptr null) #2, !dbg !734
  %132 = load ptr, ptr %131, align 8, !dbg !734
  %133 = call ptr @llvm.objc.retain(ptr %132), !dbg !734
  call void @swift_endAccess(ptr %12) #2, !dbg !708
  call void @llvm.lifetime.end.p0(i64 -1, ptr %12), !dbg !708
  %134 = ptrtoint ptr %132 to i64, !dbg !708
  %135 = load ptr, ptr @"\01L_selector(setPlayer:)", align 8, !dbg !735
  %136 = inttoptr i64 %134 to ptr, !dbg !735
  call void @objc_msgSend(ptr %130, ptr %135, ptr %136), !dbg !735
  %137 = inttoptr i64 %134 to ptr, !dbg !731
  call void @llvm.objc.release(ptr %137), !dbg !731
  call void @llvm.objc.release(ptr %130), !dbg !731
  %138 = call swiftcc %swift.metadata_response @"$sSo12AVPlayerItemCMa"(i64 0) #19, !dbg !736
  %139 = extractvalue %swift.metadata_response %138, 0, !dbg !736
  %140 = getelementptr inbounds ptr, ptr %34, i32 2, !dbg !737
  %141 = load ptr, ptr %140, align 8, !dbg !737, !invariant.load !310
  %142 = call ptr %141(ptr noalias %37, ptr noalias %38, ptr %32) #2, !dbg !737
  %143 = call swiftcc ptr @"$sSo12AVPlayerItemC3urlAB10Foundation3URLV_tcfC"(ptr noalias %37, ptr swiftself %139), !dbg !736
  store ptr %143, ptr %13, align 8, !dbg !738
  %144 = call swiftcc %swift.metadata_response @"$sSo14AVPlayerLooperCMa"(i64 0) #19, !dbg !739
  %145 = extractvalue %swift.metadata_response %144, 0, !dbg !739
  %146 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 1, !dbg !740
  call void @llvm.lifetime.start.p0(i64 -1, ptr %14), !dbg !740
  call void @swift_beginAccess(ptr %146, ptr %14, i64 32, ptr null) #2, !dbg !740
  %147 = load ptr, ptr %146, align 8, !dbg !740
  %148 = call ptr @llvm.objc.retain(ptr %147), !dbg !740
  call void @swift_endAccess(ptr %14) #2, !dbg !738
  call void @llvm.lifetime.end.p0(i64 -1, ptr %14), !dbg !738
  %149 = call ptr @llvm.objc.retain(ptr %143), !dbg !741
  %150 = call swiftcc ptr @"$sSo14AVPlayerLooperC6player12templateItemABSo13AVQueuePlayerC_So0aE0CtcfCTO"(ptr %147, ptr %143, ptr swiftself %145), !dbg !739
  %151 = ptrtoint ptr %150 to i64, !dbg !738
  %152 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 3, !dbg !742
  call void @llvm.lifetime.start.p0(i64 -1, ptr %15), !dbg !742
  call void @swift_beginAccess(ptr %152, ptr %15, i64 33, ptr null) #2, !dbg !742
  %153 = load i64, ptr %152, align 8, !dbg !742
  store i64 %151, ptr %152, align 8, !dbg !742
  %154 = inttoptr i64 %153 to ptr, !dbg !742
  call void @llvm.objc.release(ptr %154), !dbg !742
  call void @swift_endAccess(ptr %15) #2, !dbg !738
  call void @llvm.lifetime.end.p0(i64 -1, ptr %15), !dbg !738
  %155 = call swiftcc i1 @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO9isLoopingSbvg"(ptr noalias swiftself %0), !dbg !743
  %156 = xor i1 %155, true, !dbg !745
  br i1 %156, label %157, label %169, !dbg !745

157:                                              ; preds = %120
  %158 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 3, !dbg !746
  call void @llvm.lifetime.start.p0(i64 -1, ptr %20), !dbg !746
  call void @swift_beginAccess(ptr %158, ptr %20, i64 32, ptr null) #2, !dbg !746
  %159 = load i64, ptr %158, align 8, !dbg !746
  %160 = icmp eq i64 %159, 0, !dbg !746
  %161 = xor i1 %160, true, !dbg !746
  br i1 %161, label %162, label %168, !dbg !746

162:                                              ; preds = %157
  %163 = load ptr, ptr %158, align 8, !dbg !746
  %164 = call ptr @llvm.objc.retain(ptr %163), !dbg !746
  call void @swift_endAccess(ptr %20) #2, !dbg !748
  call void @llvm.lifetime.end.p0(i64 -1, ptr %20), !dbg !748
  %165 = load ptr, ptr @"\01L_selector(disableLooping)", align 8, !dbg !749
  call void @objc_msgSend(ptr %163, ptr %165), !dbg !749
  call void @llvm.objc.release(ptr %163), !dbg !750
  br label %166, !dbg !748

166:                                              ; preds = %168, %162
  %167 = phi i8 [ 1, %168 ], [ 0, %162 ], !dbg !752
  br label %170, !dbg !752

168:                                              ; preds = %157
  call void @swift_endAccess(ptr %20) #2, !dbg !748
  call void @llvm.lifetime.end.p0(i64 -1, ptr %20), !dbg !748
  br label %166, !dbg !748

169:                                              ; preds = %120
  br label %170, !dbg !753

170:                                              ; preds = %169, %166
  %171 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 3, !dbg !754
  call void @llvm.lifetime.start.p0(i64 -1, ptr %16), !dbg !754
  call void @swift_beginAccess(ptr %171, ptr %16, i64 32, ptr null) #2, !dbg !754
  %172 = load i64, ptr %171, align 8, !dbg !754
  %173 = icmp eq i64 %172, 0, !dbg !754
  %174 = xor i1 %173, true, !dbg !754
  br i1 %174, label %175, label %207, !dbg !754

175:                                              ; preds = %170
  %176 = load ptr, ptr %171, align 8, !dbg !754
  %177 = call ptr @llvm.objc.retain(ptr %176), !dbg !754
  call void @swift_endAccess(ptr %16) #2, !dbg !738
  call void @llvm.lifetime.end.p0(i64 -1, ptr %16), !dbg !738
  call void @llvm.lifetime.start.p0(i64 8, ptr %18), !dbg !755
  store ptr %176, ptr %18, align 8, !dbg !738
  %178 = call ptr @swift_getKeyPath(ptr @keypath, ptr undef) #2, !dbg !757
  %179 = load ptr, ptr %178, align 8, !dbg !738
  %180 = call swiftcc %swift.metadata_response @"$sSo26NSKeyValueObservingOptionsVMa"(i64 0) #19, !dbg !758
  %181 = extractvalue %swift.metadata_response %180, 0, !dbg !758
  %182 = call swiftcc { ptr, ptr } @"$ss27_allocateUninitializedArrayySayxG_BptBwlF"(i64 2, ptr %181), !dbg !758
  %183 = extractvalue { ptr, ptr } %182, 0, !dbg !758
  %184 = extractvalue { ptr, ptr } %182, 1, !dbg !758
  %185 = getelementptr inbounds i8, ptr %183, i64 32, !dbg !758
  %186 = getelementptr inbounds nuw %TSo26NSKeyValueObservingOptionsV, ptr %185, i32 0, i32 0, !dbg !759
  %187 = getelementptr inbounds nuw %TSu, ptr %186, i32 0, i32 0, !dbg !759
  store i64 2, ptr %187, align 8, !dbg !759
  %188 = getelementptr inbounds %TSo26NSKeyValueObservingOptionsV, ptr %185, i64 1, !dbg !758
  %189 = getelementptr inbounds nuw %TSo26NSKeyValueObservingOptionsV, ptr %188, i32 0, i32 0, !dbg !760
  %190 = getelementptr inbounds nuw %TSu, ptr %189, i32 0, i32 0, !dbg !760
  store i64 1, ptr %190, align 8, !dbg !760
  %191 = call swiftcc ptr @"$ss27_finalizeUninitializedArrayySayxGABnlF"(ptr %183, ptr %181), !dbg !758
  call void @llvm.lifetime.start.p0(i64 8, ptr %19), !dbg !755
  %192 = call ptr @"$sSo26NSKeyValueObservingOptionsVABs10SetAlgebraSCWl"() #19, !dbg !758
  call swiftcc void @"$ss10SetAlgebraPs7ElementQz012ArrayLiteralC0RtzrlE05arrayE0xAFd_tcfC"(ptr noalias sret(%swift.opaque) %19, ptr %191, ptr %181, ptr %192, ptr swiftself %181), !dbg !758
  %193 = getelementptr inbounds nuw %TSo26NSKeyValueObservingOptionsV, ptr %19, i32 0, i32 0, !dbg !758
  %194 = getelementptr inbounds nuw %TSu, ptr %193, i32 0, i32 0, !dbg !758
  %195 = load i64, ptr %194, align 8, !dbg !758
  %196 = call ptr @swift_retain(ptr returned %1) #9, !dbg !738
  %197 = call swiftcc ptr @"$s10Foundation27_KeyValueCodingAndObservingPAAE7observe_7options13changeHandlerAA05NSKeyC11ObservationCs0B4PathCyxqd__G_So0kcF7OptionsVyx_AA0kC14ObservedChangeVyqd__GtctlF"(ptr %178, i64 %195, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_TA", ptr %1, ptr @"$sSo8NSObjectC10Foundation27_KeyValueCodingAndObservingACWP", ptr noalias swiftself %18), !dbg !761
  call void @swift_release(ptr %1) #2, !dbg !762
  call void @swift_release(ptr %178) #2, !dbg !762
  call void @llvm.lifetime.end.p0(i64 8, ptr %19), !dbg !762
  %198 = load ptr, ptr %18, align 8, !dbg !762
  call void @llvm.objc.release(ptr %198), !dbg !762
  call void @llvm.lifetime.end.p0(i64 8, ptr %18), !dbg !762
  %199 = ptrtoint ptr %197 to i64, !dbg !738
  br label %200, !dbg !738

200:                                              ; preds = %207, %175
  %201 = phi i64 [ 0, %207 ], [ %199, %175 ], !dbg !764
  %202 = getelementptr inbounds nuw %T16ReproKvoVideoLib24ReproVideoPlayerProviderC, ptr %1, i32 0, i32 4, !dbg !764
  call void @llvm.lifetime.start.p0(i64 -1, ptr %17), !dbg !764
  call void @swift_beginAccess(ptr %202, ptr %17, i64 33, ptr null) #2, !dbg !764
  %203 = load i64, ptr %202, align 8, !dbg !764
  store i64 %201, ptr %202, align 8, !dbg !764
  %204 = inttoptr i64 %203 to ptr, !dbg !764
  call void @llvm.objc.release(ptr %204), !dbg !764
  call void @swift_endAccess(ptr %17) #2, !dbg !738
  call void @llvm.lifetime.end.p0(i64 -1, ptr %17), !dbg !738
  call void @llvm.objc.release(ptr %143), !dbg !765
  %205 = getelementptr inbounds ptr, ptr %34, i32 1, !dbg !765
  %206 = load ptr, ptr %205, align 8, !dbg !765, !invariant.load !310
  call void %206(ptr noalias %38, ptr %32) #2, !dbg !765
  br label %211, !dbg !765

207:                                              ; preds = %170
  call void @swift_endAccess(ptr %16) #2, !dbg !738
  call void @llvm.lifetime.end.p0(i64 -1, ptr %16), !dbg !738
  br label %200, !dbg !738

208:                                              ; preds = %2
  %209 = call ptr @"$s10Foundation3URLVSgWOh"(ptr %30), !dbg !766
  %210 = call swiftcc i1 @"$s13FkAEqqqqqqqqq0A7MustFix_10allThreads4file4line8functionySSyXK_Sbs12StaticStringVSuAHtFfA0_"(), !dbg !768
  call swiftcc void @"$s13FkAEqqqqqqqqq0A7MustFix_10allThreads4file4line8functionySSyXK_Sbs12StaticStringVSuAHtF"(ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFSSyXEfu_", ptr null, i1 %210, i64 ptrtoint (ptr @".str.106.vendor/Libraries/FkASqqqqqqq/Capabilities/ReproKvoVideoLib/ReproKvoVideoLib/ReproVideoPlayerProvider.swift" to i64), i64 106, i8 2, i64 172, i64 ptrtoint (ptr @".str.18.setupPlayer(item:)" to i64), i64 18, i8 2), !dbg !770
  br label %211, !dbg !771

211:                                              ; preds = %208, %200
  call void @llvm.lifetime.end.p0(i64 -1, ptr %38), !dbg !765
  call void @llvm.lifetime.end.p0(i64 -1, ptr %37), !dbg !765
  call void @llvm.lifetime.end.p0(i64 -1, ptr %30), !dbg !765
  ret void, !dbg !765
}
define internal swiftcc { i64, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFSSyXEfu_"() #0 !dbg !1147 {
  %1 = call swiftcc { i64, ptr } @"$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC"(ptr @".str.42.Video playback item must have a valid URL.", i64 42, i1 true), !dbg !1148
  %2 = extractvalue { i64, ptr } %1, 0, !dbg !1148
  %3 = extractvalue { i64, ptr } %1, 1, !dbg !1148
  %4 = insertvalue { i64, ptr } undef, i64 %2, 0, !dbg !1148
  %5 = insertvalue { i64, ptr } %4, ptr %3, 1, !dbg !1148
  ret { i64, ptr } %5, !dbg !1148
}
declare swiftcc void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO3url10Foundation3URLVSgvg"(ptr noalias sret(%swift.opaque), ptr noalias swiftself) #0
declare hidden swiftcc ptr @"$sSo12AVPlayerItemC3urlAB10Foundation3URLV_tcfC"(ptr noalias, ptr swiftself) #0
declare hidden swiftcc ptr @"$sSo14AVPlayerLooperC6player12templateItemABSo13AVQueuePlayerC_So0aE0CtcfCTO"(ptr, ptr, ptr swiftself) #0
declare swiftcc i1 @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqO9isLoopingSbvg"(ptr noalias swiftself) #0
declare hidden swiftcc void @"$sSo14AVPlayerLooperC6statusSo0aB6StatusVvpABTK"(ptr noalias sret(%TSo20AVPlayerLooperStatusV) captures(none), ptr noalias captures(none) dereferenceable(8), ptr, i64) #0
declare hidden swiftcc ptr @"$ss27_finalizeUninitializedArrayySayxGABnlF"(ptr, ptr) #0
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_"(ptr noalias captures(none) dereferenceable(8) %0, ptr noalias %1, ptr %2) #0 !dbg !1243 {
  %4 = alloca ptr, align 8
    #dbg_declare(ptr %4, !1248, !DIExpression(), !1253)
  call void @llvm.memset.p0.i64(ptr align 8 %4, i8 0, i64 8, i1 false)
  %5 = alloca ptr, align 8
    #dbg_declare(ptr %5, !1250, !DIExpression(DW_OP_deref), !1254)
  call void @llvm.memset.p0.i64(ptr align 8 %5, i8 0, i64 8, i1 false)
  %6 = alloca ptr, align 8
    #dbg_declare(ptr %6, !1252, !DIExpression(), !1255)
  call void @llvm.memset.p0.i64(ptr align 8 %6, i8 0, i64 8, i1 false)
  %7 = alloca i8, i64 32, align 16
  %8 = alloca swifterror ptr, align 8
  store ptr null, ptr %8, align 8
  %9 = load ptr, ptr %0, align 8, !dbg !1256
  store ptr %9, ptr %4, align 8, !dbg !1258
  store ptr %1, ptr %5, align 8, !dbg !1258
  store ptr %2, ptr %6, align 8, !dbg !1259
  %10 = call swiftcc %swift.metadata_response @"$sScMMa"(i64 0) #19, !dbg !1263
  %11 = extractvalue %swift.metadata_response %10, 0, !dbg !1263
  %12 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata.20, i32 0, i32 2), i64 24, i64 7) #2, !dbg !1264
  %13 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %12, i32 0, i32 1, !dbg !1264
  %14 = call ptr @swift_retain(ptr returned %2) #9, !dbg !1267
  %15 = ptrtoint ptr %2 to i64, !dbg !1258
  %16 = inttoptr i64 %15 to ptr, !dbg !1258
  %17 = call ptr @swift_weakInit(ptr returned %13, ptr %16) #2, !dbg !1258
  %18 = inttoptr i64 %15 to ptr, !dbg !1258
  call void @swift_release(ptr %18) #2, !dbg !1258
  %19 = call ptr @swift_retain(ptr returned %12) #9, !dbg !1258
  %20 = call ptr @llvm.objc.retain(ptr %9), !dbg !1258
  call void @llvm.lifetime.start.p0(i64 32, ptr %7), !dbg !1268
  %21 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %7, i32 0, i32 1, !dbg !1268
  store ptr %12, ptr %21, align 8, !dbg !1268
  %22 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %7, i32 0, i32 2, !dbg !1268
  store ptr %9, ptr %22, align 8, !dbg !1268
  call swiftcc void @"$sScM14assumeIsolated_4file4linexxyKScMYcXE_s12StaticStringVSutKs8SendableRzlFZ"(ptr noalias sret(%swift.opaque) undef, ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_TA", ptr %7, i64 ptrtoint (ptr @".str.47.ReproKvoVideoLib/ReproVideoPlayerProvider.swift" to i64), i64 47, i8 2, i64 206, ptr getelementptr inbounds (%swift.full_existential_type, ptr @"$sytN", i32 0, i32 1), ptr swiftself %11, ptr noalias swifterror captures(none) dereferenceable(8) %8), !dbg !1269
  %23 = load ptr, ptr %8, align 8, !dbg !1269
  %24 = icmp ne ptr %23, null, !dbg !1269
  %25 = ptrtoint ptr %23 to i64, !dbg !1269
  br i1 %24, label %27, label %26, !dbg !1269

26:                                               ; preds = %3
  call void @llvm.lifetime.end.p0(i64 32, ptr %7), !dbg !1270
  call void @swift_release(ptr %12) #2, !dbg !1270
  call void @llvm.objc.release(ptr %9), !dbg !1270
  call void @swift_release(ptr %12) #2, !dbg !1270
  ret void, !dbg !1270

27:                                               ; preds = %3
  %28 = phi ptr [ %23, %3 ], !dbg !1258
  store ptr null, ptr %8, align 8, !dbg !1269
  call void @swift_errorRelease(ptr %28) #2, !dbg !1258
  call void @llvm.lifetime.end.p0(i64 32, ptr %7), !dbg !1258
  call void @swift_release(ptr %12) #2, !dbg !1269
  call void @llvm.objc.release(ptr %9), !dbg !1269
  unreachable, !dbg !1269
}
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_"(ptr noalias sret(%swift.opaque) captures(none) %0, ptr %1, ptr %2, ptr swiftself %3, ptr noalias swifterror captures(none) dereferenceable(8) %4) #0 !dbg !1271 {
  %6 = alloca ptr, align 8
    #dbg_declare(ptr %6, !1276, !DIExpression(DW_OP_deref), !1290)
  call void @llvm.memset.p0.i64(ptr align 8 %6, i8 0, i64 8, i1 false)
  %7 = alloca ptr, align 8
    #dbg_declare(ptr %7, !1284, !DIExpression(), !1291)
  call void @llvm.memset.p0.i64(ptr align 8 %7, i8 0, i64 8, i1 false)
  %8 = alloca [24 x i8], align 8
  %9 = alloca ptr, align 8
    #dbg_declare(ptr %9, !1285, !DIExpression(), !1292)
  call void @llvm.memset.p0.i64(ptr align 8 %9, i8 0, i64 8, i1 false)
  %10 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %1, i32 0, i32 1, !dbg !1290
  store ptr %10, ptr %6, align 8, !dbg !1293
  store ptr %2, ptr %7, align 8, !dbg !1294
  call void @llvm.lifetime.start.p0(i64 -1, ptr %8), !dbg !1295
  call void @swift_beginAccess(ptr %10, ptr %8, i64 32, ptr null) #2, !dbg !1295
  %11 = call ptr @swift_weakLoadStrong(ptr %10) #2, !dbg !1295
  %12 = ptrtoint ptr %11 to i64, !dbg !1295
  call void @swift_endAccess(ptr %8) #2, !dbg !1293
  call void @llvm.lifetime.end.p0(i64 -1, ptr %8), !dbg !1293
  %13 = icmp eq i64 %12, 0, !dbg !1295
  br i1 %13, label %16, label %14, !dbg !1295

14:                                               ; preds = %5
  %15 = inttoptr i64 %12 to ptr, !dbg !1295
  br label %17, !dbg !1295

16:                                               ; preds = %5
  br label %35, !dbg !1296

17:                                               ; preds = %14
  %18 = phi ptr [ %15, %14 ], !dbg !1298
  store ptr %18, ptr %9, align 8, !dbg !1300
  %19 = load ptr, ptr @"\01L_selector(status)", align 8, !dbg !1301
  %20 = call i64 @objc_msgSend(ptr %2, ptr %19), !dbg !1301
  %21 = load ptr, ptr @"\01L_selector(error)", align 8, !dbg !1302
  %22 = call ptr @objc_msgSend(ptr %2, ptr %21), !dbg !1302
  call void asm sideeffect "mov\09fp, fp\09\09// marker for objc_retainAutoreleaseReturnValue", ""(), !dbg !1302
  %23 = call ptr @llvm.objc.retainAutoreleasedReturnValue(ptr %22), !dbg !1302
  %24 = ptrtoint ptr %23 to i64, !dbg !1302
  %25 = icmp eq i64 %24, 0, !dbg !1302
  br i1 %25, label %31, label %26, !dbg !1302

26:                                               ; preds = %17
  %27 = inttoptr i64 %24 to ptr, !dbg !1302
  br label %28, !dbg !1302

28:                                               ; preds = %26
  %29 = phi ptr [ %27, %26 ], !dbg !1302
  %30 = ptrtoint ptr %29 to i64, !dbg !1302
  br label %32, !dbg !1302

31:                                               ; preds = %17
  br label %32, !dbg !1302

32:                                               ; preds = %31, %28
  %33 = phi i64 [ 0, %31 ], [ %30, %28 ], !dbg !1303
  call swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC21didUpdateLooperStatus33_DE9A0A2FE3FAF13552D292CAA2A5386FLL_5errorySo0dhI0V_s5Error_pSgtF"(i64 %20, i64 %33, ptr swiftself %18), !dbg !1303
  %34 = inttoptr i64 %33 to ptr, !dbg !1304
  call void @swift_errorRelease(ptr %34) #2, !dbg !1304
  call void @swift_release(ptr %18) #2, !dbg !1304
  br label %35, !dbg !1304

35:                                               ; preds = %32, %16
  ret void, !dbg !1304
}
declare swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC21didUpdateLooperStatus33_DE9A0A2FE3FAF13552D292CAA2A5386FLL_5errorySo0dhI0V_s5Error_pSgtF"(i64, i64, ptr swiftself) #0
declare hidden swiftcc void @"$sScM14assumeIsolated_4file4linexxyKScMYcXE_s12StaticStringVSutKs8SendableRzlFZ"(ptr noalias sret(%swift.opaque), ptr, ptr, i64, i64, i8, i64, ptr, ptr swiftself, ptr noalias swifterror captures(none) dereferenceable(8)) #0
declare hidden ptr @"$sSo14AVPlayerLooperCSg_ACtWOh"(ptr) #7
declare hidden swiftcc %swift.metadata_response @"$sSo12AVPlayerItemCMa"(i64) #1
declare hidden swiftcc %swift.metadata_response @"$sSo14AVPlayerLooperCMa"(i64) #1
declare hidden ptr @keypath_get_selector_status(ptr) #14
declare ptr @swift_getKeyPath(ptr, ptr) #2
declare swiftcc { ptr, ptr } @"$ss27_allocateUninitializedArrayySayxG_BptBwlF"(i64, ptr) #0
declare hidden swiftcc %swift.metadata_response @"$sSo26NSKeyValueObservingOptionsVMa"(i64) #1
declare swiftcc void @"$ss10SetAlgebraPs7ElementQz012ArrayLiteralC0RtzrlE05arrayE0xAFd_tcfC"(ptr noalias sret(%swift.opaque), ptr, ptr, ptr, ptr swiftself) #0
declare hidden ptr @"$sSo26NSKeyValueObservingOptionsVABs10SetAlgebraSCWl"() #1
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_TA"(ptr noalias captures(none) dereferenceable(8) %0, ptr noalias %1, ptr swiftself %2) #0 !dbg !1401 {
  tail call swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_"(ptr noalias captures(none) dereferenceable(8) %0, ptr noalias %1, ptr %2), !dbg !1402
  ret void, !dbg !1402
}
declare swiftcc ptr @"$s10Foundation27_KeyValueCodingAndObservingPAAE7observe_7options13changeHandlerAA05NSKeyC11ObservationCs0B4PathCyxqd__G_So0kcF7OptionsVyx_AA0kC14ObservedChangeVyqd__GtctlF"(ptr, i64, ptr, ptr, ptr, ptr noalias swiftself) #0
declare hidden ptr @"$sSo14AVPlayerLooperCSgWOc"(ptr, ptr) #7
declare swiftcc i1 @"$sSo8NSObjectC10ObjectiveCE2eeoiySbAB_ABtFZ"(ptr, ptr, ptr swiftself) #0
declare swiftcc %swift.metadata_response @"$sScMMa"(i64) #0
declare void @swift_errorRelease(ptr) #2
declare hidden swiftcc { i64, i64 } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvsTW"(i64, i64, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc { ptr, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP8delegateAA0cF8Delegate_pSgvMTW"(ptr noalias dereferenceable(32), ptr swiftself, ptr, ptr) #5
declare hidden swiftcc ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP11playerLayerSo7CALayerCvgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avsTW"(ptr, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc { ptr, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12videoGravitySo07AVLayerbH0avMTW"(ptr noalias dereferenceable(32), ptr swiftself, ptr, ptr) #5
declare hidden swiftcc i1 @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvsTW"(i1, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc { ptr, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP7isMutedSbvMTW"(ptr noalias dereferenceable(32), ptr swiftself, ptr, ptr) #5
declare hidden swiftcc i8 @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP6statusAA0C14PlaybackStatusOvgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvgTW"(ptr noalias sret(%swift.opaque), ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvsTW"(ptr noalias, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc { ptr, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP12playbackItemAA0c8PlaybackH0OSgvMTW"(ptr noalias dereferenceable(32), ptr swiftself, ptr, ptr) #5
declare hidden swiftcc { i64, ptr } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP16debugDescriptionSSvgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc { i64, i64, i64 } @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP11currentTimeSo6CMTimeavgTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4playyyFTW"(ptr swiftself, ptr, ptr) #0
declare hidden swifttailcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4play9timeRangeySo06CMTimeI0a_tYaFTW"(ptr swiftasync, ptr noalias captures(none) dereferenceable(48), ptr swiftself, ptr, ptr) #16
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP5pauseyyFTW"(ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP4seek2toySo6CMTimea_tFTW"(i64, i64, i64, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP16addDisplayEffectyyyXlFTW"(ptr, ptr swiftself, ptr, ptr) #0
declare hidden swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCAA0c6PlayerE0A2aDP19removeDisplayEffectyyyXlFTW"(ptr, ptr swiftself, ptr, ptr) #0
declare swiftcc %swift.metadata_response @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMa"(i64) #1
declare ptr @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMU"(ptr, ptr) #0
declare swiftcc %swift.metadata_response @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderCMr"(ptr, ptr, ptr) #14
declare ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwCP"(ptr noalias, ptr noalias, ptr) #14
declare void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwxx"(ptr noalias, ptr) #14
declare ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwcp"(ptr noalias, ptr noalias, ptr) #14
declare ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwca"(ptr, ptr, ptr) #14
declare ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwtk"(ptr noalias, ptr noalias, ptr) #14
declare ptr @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwta"(ptr noalias, ptr noalias, ptr) #14
declare i32 @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwet"(ptr noalias, i32, ptr) #20
declare void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwst"(ptr noalias, i32, i32, ptr) #14
declare i32 @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwug"(ptr noalias, ptr) #14
declare void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwup"(ptr noalias, ptr) #14
declare void @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOwui"(ptr noalias, i32, ptr) #14
declare swiftcc %swift.metadata_response @"$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOMr"(ptr, ptr, ptr) #14
declare swiftcc %swift.metadata_response @"$s16ReproKvoVideoLib30FkAAqqqqqqqqqqqqqqqqqqqqqqqqqqVMa"(i64) #1
declare hidden swiftcc %swift.metadata_response @"$sSo6CMTimeaMa"(i64) #1
declare hidden swiftcc %swift.metadata_response @"$sSo20AVPlayerLooperStatusVMa"(i64) #1
declare swiftcc void @objectdestroy.19(ptr swiftself) #0
declare ptr @swift_weakInit(ptr returned, ptr) #9
declare ptr @swift_weakLoadStrong(ptr) #9
define internal swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_TA"(ptr noalias sret(%swift.opaque) captures(none) %0, ptr swiftself %1, ptr noalias swifterror captures(none) dereferenceable(8) %2) #0 !dbg !3121 {
  %4 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %1, i32 0, i32 1, !dbg !3122
  %5 = load ptr, ptr %4, align 8, !dbg !3122
  %6 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %1, i32 0, i32 2, !dbg !3122
  %7 = load ptr, ptr %6, align 8, !dbg !3122
  tail call swiftcc void @"$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_"(ptr noalias sret(%swift.opaque) captures(none) %0, ptr %5, ptr %7, ptr swiftself undef, ptr noalias swifterror captures(none) dereferenceable(8) %2), !dbg !3122
  ret void, !dbg !3122
}
declare hidden swiftcc %swift.metadata_response @"$sSo11CMTimeFlagsVMa"(i64) #1
attributes #0 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #1 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #2 = { nounwind }
attributes #3 = { nounwind willreturn memory(none) }
attributes #4 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #5 = { noinline "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #6 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #7 = { noinline nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #8 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #9 = { nounwind willreturn }
attributes #10 = { nounwind willreturn memory(read) }
attributes #11 = { noinline nounwind willreturn memory(read) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #12 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #13 = { nounwind memory(argmem: readwrite) }
attributes #14 = { nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #15 = { nounwind memory(read) }
attributes #16 = { noinline "async_entry" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #17 = { noinline "async_ret" "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #18 = { nounwind memory(inaccessiblemem: readwrite) }
attributes #19 = { nounwind memory(none) }
attributes #20 = { nounwind memory(read) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #21 = { nounwind willreturn memory(argmem: readwrite) }
attributes #22 = { cold noreturn nounwind memory(inaccessiblemem: write) }
attributes #23 = { noinline }
attributes #24 = { nounwind memory(argmem: read) }
!llvm.module.flags = !{!12, !13, !14, !15, !16, !17, !18, !19, !20, !21, !22, !23, !24, !25, !26, !27, !28, !29}
!llvm.dbg.cu = !{!30, !86, !88, !90, !92, !94, !96, !98, !100, !102, !104, !106}
!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "preferredTimescale", linkageName: "$s16ReproKvoVideoLib23FkAFqqqqqqqqqqqqqqqqqqqV18preferredTimescales5Int32VvpZ", scope: !2, file: !3, line: 16, type: !4, isLocal: false, isDefinition: true)
!2 = !DIModule(scope: null, name: "ReproKvoVideoLib", includePath: "build/cache/")
!3 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!4 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !5)
!5 = !DIDerivedType(tag: DW_TAG_typedef, name: "$sSo11CMTimeScaleaD", scope: !7, file: !6, baseType: !9)
!6 = !DIFile(filename: "build/cache/", directory: ".")
!7 = !DIModule(scope: !8, name: "CMTime", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!8 = !DIModule(scope: null, name: "CoreMedia", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!9 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int32", scope: !11, file: !10, size: 32, runtimeLang: DW_LANG_Swift, identifier: "$ss5Int32VD")
!10 = !DIFile(filename: "build/cache/", directory: ".")
!11 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!12 = !{i32 2, !"SDK Version", [2 x i32] [i32 26, i32 2]}
!13 = !{i32 1, !"Objective-C Version", i32 2}
!14 = !{i32 1, !"Objective-C Image Info Version", i32 0}
!15 = !{i32 1, !"Objective-C Image Info Section", !"__DATA,__objc_imageinfo,regular,no_dead_strip"}
!16 = !{i32 1, !"Objective-C Garbage Collection", i8 0}
!17 = !{i32 1, !"Objective-C Is Simulated", i32 32}
!18 = !{i32 1, !"Objective-C Class Properties", i32 64}
!19 = !{i32 1, !"Objective-C Enforce ClassRO Pointer Signing", i8 0}
!20 = !{i32 7, !"Dwarf Version", i32 4}
!21 = !{i32 2, !"Debug Info Version", i32 3}
!22 = !{i32 1, !"wchar_size", i32 4}
!23 = !{i32 8, !"PIC Level", i32 2}
!24 = !{i32 7, !"uwtable", i32 1}
!25 = !{i32 7, !"frame-pointer", i32 1}
!26 = !{i32 1, !"Swift Version", i32 7}
!27 = !{i32 1, !"Swift ABI Version", i32 7}
!28 = !{i32 1, !"Swift Major Version", i8 6}
!29 = !{i32 1, !"Swift Minor Version", i8 3}
!30 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !31, producer: "Swift version (redacted)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, retainedTypes: !32, globals: !37, imports: !38, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!31 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!32 = !{!5, !33}
!33 = !DIDerivedType(tag: DW_TAG_typedef, name: "$ss9AnyObjectaD", scope: !11, file: !10, baseType: !34)
!34 = !DIDerivedType(tag: DW_TAG_typedef, name: "AnyObject", scope: !35, file: !31, baseType: !36)
!35 = !DIModule(scope: null, name: "Builtin", configMacros: "\22-DDEBUG=1\22")
!36 = !DICompositeType(tag: DW_TAG_structure_type, name: "$syXlD", size: 64, runtimeLang: DW_LANG_Swift, identifier: "$syXlD")
!37 = !{!0}
!38 = !{!39, !40, !41, !43, !46, !48, !50, !53, !55, !57, !59, !60, !61, !63, !64, !66, !67, !68, !69, !71, !72, !74, !76, !78, !80, !82, !84}
!39 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !2, file: !31)
!40 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: null, entity: !11)
!41 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !42, file: !31, line: 3)
!42 = !DIModule(scope: null, name: "FkAOqqqqqqqqqqq", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!43 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !44, entity: !45, file: !44)
!44 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!45 = !DIModule(scope: null, name: "AVFoundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!46 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !44, entity: !47, file: !44, line: 3)
!47 = !DIModule(scope: null, name: "AVFoundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!48 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !44, entity: !49, file: !44, line: 4)
!49 = !DIModule(scope: null, name: "FkAEqqqqqqqqq", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!50 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !51, entity: !52, file: !51)
!51 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!52 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!53 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !51, entity: !54, file: !51, line: 3)
!54 = !DIModule(scope: null, name: "Foundation", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!55 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !56, entity: !8, file: !56)
!56 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!57 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !56, entity: !58, file: !56, line: 3)
!58 = !DIModule(scope: null, name: "CoreMedia", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!59 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !3, entity: !45, file: !3)
!60 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !3, entity: !47, file: !3, line: 3)
!61 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !62, entity: !45, file: !62)
!62 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!63 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !62, entity: !47, file: !62, line: 3)
!64 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !45, file: !65)
!65 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!66 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !47, file: !65, line: 3)
!67 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !49, file: !65, line: 4)
!68 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !42, file: !65, line: 5)
!69 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !70, file: !65)
!70 = !DIModule(scope: null, name: "FkAUqqqqq", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!71 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !70, file: !65, line: 6)
!72 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !65, entity: !73, file: !65, line: 7)
!73 = !DIModule(scope: null, name: "SwiftSockets", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!74 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !75, entity: !73, file: !75, line: 4)
!75 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_kvo_observer_weak_self.swift", directory: ".")
!76 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !77, file: !31)
!77 = !DIModule(scope: null, name: "ReproKvoVideoLib", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!78 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !79, file: !31)
!79 = !DIModule(scope: null, name: "_StringProcessing", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!80 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !81, file: !31)
!81 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!82 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !83, file: !31)
!83 = !DIModule(scope: null, name: "_Concurrency", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!84 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !31, entity: !85, file: !31)
!85 = !DIModule(scope: null, name: "SwiftOnoneSupport", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!86 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !87, producer: "clang version (redacted)", isOptimized: false, runtimeVersion: 2, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: Apple, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!87 = !DIFile(filename: "<swift-imported-modules>", directory: ".")
!88 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !89, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!89 = !DIFile(filename: "CoreMedia", directory: ".")
!90 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !91, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!91 = !DIFile(filename: "AVFoundation", directory: ".")
!92 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !93, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!93 = !DIFile(filename: "QuartzCore", directory: ".")
!94 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !95, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!95 = !DIFile(filename: "CoreFoundation", directory: ".")
!96 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !97, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!97 = !DIFile(filename: "FkAPqqqqqqqqqq", directory: ".")
!98 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !99, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!99 = !DIFile(filename: "ReproKvoVideoLib", directory: ".")
!100 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !101, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!101 = !DIFile(filename: "Foundation", directory: ".")
!102 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !103, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!103 = !DIFile(filename: "UIKit", directory: ".")
!104 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !105, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!105 = !DIFile(filename: "FkAUqqqqq", directory: ".")
!106 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !107, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!107 = !DIFile(filename: "_SwiftConcurrencyShims", directory: ".")
!177 = !DIFile(filename: "<compiler-generated>", directory: "")
!184 = !DISubroutineType(types: null)
!194 = !DICompositeType(tag: DW_TAG_structure_type, name: "ReproVideoPlayerProvider", scope: !2, file: !44, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderCD")
!202 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!214 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !194)
!283 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvpfi", scope: !2, file: !177, type: !284, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !30)
!284 = !DISubroutineType(types: !285)
!285 = !{!286}
!286 = !DICompositeType(tag: DW_TAG_structure_type, name: "Optional", scope: !11, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s10Foundation21NSKeyValueObservationCSgD")
!287 = !DILocation(line: 0, scope: !283)
!288 = distinct !DISubprogram(name: "statusObserver.get", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvg", scope: !194, file: !177, type: !289, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30, declaration: !291)
!289 = !DISubroutineType(types: !290)
!290 = !{!286, !194}
!291 = !DISubprogram(name: "statusObserver.get", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvg", scope: !194, file: !177, type: !289, spFlags: DISPFlagLocalToUnit)
!292 = !DILocation(line: 0, scope: !288)
!293 = distinct !DISubprogram(name: "statusObserver.set", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvs", scope: !194, file: !177, type: !294, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30, declaration: !296)
!294 = !DISubroutineType(types: !295)
!295 = !{!202, !286, !194}
!296 = !DISubprogram(name: "statusObserver.set", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC14statusObserver33_DE9A0A2FE3FAF13552D292CAA2A5386FLL10Foundation21NSKeyValueObservationCSgvs", scope: !194, file: !177, type: !294, spFlags: DISPFlagLocalToUnit)
!297 = !DILocation(line: 0, scope: !293)
!310 = !{}
!311 = !{i64 96}
!440 = !DICompositeType(tag: DW_TAG_structure_type, name: "String", scope: !11, file: !10, size: 128, runtimeLang: DW_LANG_Swift, identifier: "$sSSD")
!519 = !DICompositeType(tag: DW_TAG_structure_type, name: "Bool", scope: !11, file: !10, size: 8, runtimeLang: DW_LANG_Swift, identifier: "$sSbD")
!534 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !519)
!560 = !DISubroutineType(types: !561)
!561 = !{!440}
!582 = !DICompositeType(tag: DW_TAG_structure_type, name: "URL", scope: !54, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s10Foundation3URLVD")
!584 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !585)
!585 = !DICompositeType(tag: DW_TAG_structure_type, name: "FkACqqqqqqqqqqqqqqqq", scope: !2, file: !51, runtimeLang: DW_LANG_Swift, identifier: "$s16ReproKvoVideoLib20FkACqqqqqqqqqqqqqqqqOD")
!667 = distinct !DISubprogram(name: "setupPlayer", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tF", scope: !194, file: !44, line: 170, type: !668, scopeLine: 170, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30, declaration: !670, retainedNodes: !671)
!668 = !DISubroutineType(types: !669)
!669 = !{!202, !585, !194}
!670 = !DISubprogram(name: "setupPlayer", linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tF", scope: !194, file: !44, line: 170, type: !668, scopeLine: 170, spFlags: DISPFlagLocalToUnit)
!671 = !{!672, !676, !677, !678, !680}
!672 = !DILocalVariable(name: "url", scope: !673, file: !31, line: 171, type: !675)
!673 = distinct !DILexicalBlock(scope: !674, file: !44, line: 171, column: 21)
!674 = distinct !DILexicalBlock(scope: !667, file: !44, line: 171, column: 5)
!675 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !582)
!676 = !DILocalVariable(name: "item", arg: 1, scope: !667, file: !31, line: 170, type: !584)
!677 = !DILocalVariable(name: "self", arg: 2, scope: !667, file: !31, line: 170, type: !214, flags: DIFlagArtificial)
!678 = !DILocalVariable(name: "wasMuted", scope: !679, file: !31, line: 175, type: !534)
!679 = distinct !DILexicalBlock(scope: !673, file: !44, line: 175, column: 9)
!680 = !DILocalVariable(name: "playerItem", scope: !681, file: !31, line: 191, type: !682)
!681 = distinct !DILexicalBlock(scope: !679, file: !44, line: 191, column: 9)
!682 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !683)
!683 = !DICompositeType(tag: DW_TAG_structure_type, name: "AVPlayerItem", scope: !685, file: !684, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSo12AVPlayerItemCD")
!684 = !DIFile(filename: "build/cache/", directory: ".")
!685 = !DIModule(scope: !45, name: "AVPlayerItem", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!686 = !DILocation(line: 171, column: 15, scope: !673)
!687 = !DILocation(line: 170, column: 28, scope: !667)
!688 = !DILocation(line: 170, column: 16, scope: !667)
!689 = !DILocation(line: 175, column: 9, scope: !679)
!690 = !DILocation(line: 191, column: 9, scope: !681)
!691 = !DILocation(line: 0, scope: !692)
!692 = !DILexicalBlockFile(scope: !693, discriminator: 0)
!693 = distinct !DILexicalBlock(scope: !674, file: !44, line: 171, column: 21)
!694 = !DILocation(line: 0, scope: !695)
!695 = !DILexicalBlockFile(scope: !696, discriminator: 0)
!696 = distinct !DILexicalBlock(scope: !679, file: !44, line: 191, column: 22)
!697 = !DILocation(line: 0, scope: !698)
!698 = !DILexicalBlockFile(scope: !673, discriminator: 0)
!699 = !DILocation(line: 0, scope: !673)
!700 = !DILocation(line: 0, scope: !667)
!701 = !DILocation(line: 171, column: 26, scope: !693)
!702 = !DILocation(line: 175, column: 20, scope: !703)
!703 = distinct !DILexicalBlock(scope: !673, file: !44, line: 175, column: 20)
!704 = !DILocation(line: 0, scope: !703)
!705 = !DILocation(line: 175, column: 32, scope: !703)
!706 = !DILocation(line: 0, scope: !707)
!707 = !DILexicalBlockFile(scope: !679, discriminator: 0)
!708 = !DILocation(line: 0, scope: !679)
!709 = !DILocation(line: 176, column: 8, scope: !710)
!710 = distinct !DILexicalBlock(scope: !679, file: !44, line: 176, column: 5)
!711 = !DILocation(line: 0, scope: !710)
!712 = !DILocation(line: 0, scope: !713)
!713 = !DILexicalBlockFile(scope: !710, discriminator: 0)
!714 = !DILocation(line: 176, column: 15, scope: !710)
!715 = !DILocation(line: 178, column: 7, scope: !716)
!716 = distinct !DILexicalBlock(scope: !710, file: !44, line: 176, column: 22)
!717 = !DILocation(line: 179, column: 15, scope: !716)
!718 = !DILocation(line: 0, scope: !719)
!719 = !DILexicalBlockFile(scope: !716, discriminator: 0)
!720 = !DILocation(line: 0, scope: !721)
!721 = !DILexicalBlockFile(scope: !716, file: !177, discriminator: 0)
!722 = !DILocation(line: 0, scope: !716)
!723 = !DILocation(line: 179, column: 43, scope: !716)
!724 = !DILocation(line: 179, column: 17, scope: !716)
!725 = !DILocation(line: 180, column: 5, scope: !716)
!726 = !DILocation(line: 176, column: 5, scope: !710)
!727 = !DILocation(line: 188, column: 19, scope: !679)
!728 = !DILocation(line: 188, column: 17, scope: !679)
!729 = !DILocation(line: 189, column: 5, scope: !679)
!730 = !DILocation(line: 189, column: 25, scope: !679)
!731 = !DILocation(line: 0, scope: !732)
!732 = !DILexicalBlockFile(scope: !679, file: !177, discriminator: 0)
!733 = !DILocation(line: 190, column: 5, scope: !679)
!734 = !DILocation(line: 190, column: 31, scope: !679)
!735 = !DILocation(line: 190, column: 29, scope: !679)
!736 = !DILocation(line: 191, column: 22, scope: !696)
!737 = !DILocation(line: 191, column: 40, scope: !696)
!738 = !DILocation(line: 0, scope: !681)
!739 = !DILocation(line: 193, column: 14, scope: !681)
!740 = !DILocation(line: 193, column: 37, scope: !681)
!741 = !DILocation(line: 193, column: 64, scope: !681)
!742 = !DILocation(line: 193, column: 12, scope: !681)
!743 = !DILocation(line: 196, column: 14, scope: !744)
!744 = distinct !DILexicalBlock(scope: !681, file: !44, line: 196, column: 5)
!745 = !DILocation(line: 196, column: 8, scope: !744)
!746 = !DILocation(line: 197, column: 13, scope: !747)
!747 = distinct !DILexicalBlock(scope: !744, file: !44, line: 196, column: 24)
!748 = !DILocation(line: 0, scope: !747)
!749 = !DILocation(line: 197, column: 15, scope: !747)
!750 = !DILocation(line: 0, scope: !751)
!751 = !DILexicalBlockFile(scope: !747, file: !177, discriminator: 0)
!752 = !DILocation(line: 198, column: 5, scope: !747)
!753 = !DILocation(line: 196, column: 5, scope: !744)
!754 = !DILocation(line: 205, column: 28, scope: !681)
!755 = !DILocation(line: 0, scope: !756)
!756 = !DILexicalBlockFile(scope: !681, discriminator: 0)
!757 = !DILocation(line: 205, column: 38, scope: !681)
!758 = !DILocation(line: 205, column: 57, scope: !681)
!759 = !DILocation(line: 205, column: 59, scope: !681)
!760 = !DILocation(line: 205, column: 65, scope: !681)
!761 = !DILocation(line: 205, column: 30, scope: !681)
!762 = !DILocation(line: 0, scope: !763)
!763 = !DILexicalBlockFile(scope: !681, file: !177, discriminator: 0)
!764 = !DILocation(line: 205, column: 20, scope: !681)
!765 = !DILocation(line: 211, column: 3, scope: !681)
!766 = !DILocation(line: 0, scope: !767)
!767 = !DILexicalBlockFile(scope: !693, file: !177, discriminator: 0)
!768 = !DILocation(line: 0, scope: !769)
!769 = distinct !DILexicalBlock(scope: !674, file: !44, line: 171, column: 35)
!770 = !DILocation(line: 172, column: 7, scope: !769)
!771 = !DILocation(line: 173, column: 7, scope: !769)
!1147 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFSSyXEfu_", scope: !667, file: !177, type: !560, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30)
!1148 = !DILocation(line: 0, scope: !1147)
!1190 = !DICompositeType(tag: DW_TAG_structure_type, name: "AVPlayerLooper", scope: !1192, file: !1191, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$sSo14AVPlayerLooperCD")
!1191 = !DIFile(filename: "build/cache/", directory: ".")
!1192 = !DIModule(scope: !45, name: "AVPlayerLooper", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!1243 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_", scope: !667, file: !44, line: 205, type: !1244, scopeLine: 205, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30, retainedNodes: !1247)
!1244 = !DISubroutineType(types: !1245)
!1245 = !{!202, !1190, !1246, !194}
!1246 = !DICompositeType(tag: DW_TAG_structure_type, name: "NSKeyValueObservedChange", scope: !54, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s10Foundation24NSKeyValueObservedChangeVySo20AVPlayerLooperStatusVGD")
!1247 = !{!1248, !1250, !1252}
!1248 = !DILocalVariable(name: "looper", arg: 1, scope: !1243, file: !31, line: 205, type: !1249)
!1249 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1190)
!1250 = !DILocalVariable(name: "_1", arg: 2, scope: !1243, file: !31, line: 205, type: !1251)
!1251 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1246)
!1252 = !DILocalVariable(name: "self", arg: 3, scope: !1243, file: !31, line: 170, type: !214, flags: DIFlagArtificial)
!1253 = !DILocation(line: 205, column: 73, scope: !1243)
!1254 = !DILocation(line: 205, column: 81, scope: !1243)
!1255 = !DILocation(line: 170, column: 16, scope: !1243)
!1256 = !DILocation(line: 0, scope: !1257)
!1257 = !DILexicalBlockFile(scope: !1243, discriminator: 0)
!1258 = !DILocation(line: 0, scope: !1243)
!1259 = !DILocation(line: 0, scope: !1260)
!1260 = distinct !DILexicalBlock(scope: !1261, file: !44, line: 170, column: 3)
!1261 = distinct !DILexicalBlock(scope: !1262, file: !44, line: 10, column: 69)
!1262 = distinct !DILexicalBlock(scope: !1243, file: !44, line: 9, column: 1)
!1263 = !DILocation(line: 206, column: 7, scope: !1243)
!1264 = !DILocation(line: 0, scope: !1265)
!1265 = !DILexicalBlockFile(scope: !1266, discriminator: 0)
!1266 = distinct !DILexicalBlock(scope: !1243, file: !44, line: 207, column: 9)
!1267 = !DILocation(line: 206, column: 40, scope: !1243)
!1268 = !DILocation(line: 206, column: 32, scope: !1243)
!1269 = !DILocation(line: 206, column: 17, scope: !1243)
!1270 = !DILocation(line: 210, column: 5, scope: !1243)
!1271 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_", scope: !667, file: !44, line: 206, type: !1272, scopeLine: 206, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30, retainedNodes: !1275, thrownTypes: !1288)
!1272 = !DISubroutineType(types: !1273)
!1273 = !{!202, !1274, !1190}
!1274 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderCSgXwz_XxD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderCSgXwz_XxD")
!1275 = !{!1276, !1284, !1285}
!1276 = !DILocalVariable(name: "self", arg: 2, scope: !1271, file: !31, line: 206, type: !1277, flags: DIFlagArtificial)
!1277 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderCXSqXwD", scope: !31, baseType: !1278)
!1278 = !DICompositeType(tag: DW_TAG_structure_type, scope: !11, file: !10, size: 64, elements: !1279, runtimeLang: DW_LANG_Swift)
!1279 = !{!1280}
!1280 = !DIDerivedType(tag: DW_TAG_member, scope: !11, file: !10, baseType: !1281, size: 64)
!1281 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderCSgD", scope: !11, file: !10, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: !1282)
!1282 = !{!1283}
!1283 = !DITemplateTypeParameter(type: !194)
!1284 = !DILocalVariable(name: "looper", arg: 3, scope: !1271, file: !31, line: 205, type: !1249)
!1285 = !DILocalVariable(name: "self", scope: !1286, file: !31, line: 207, type: !214)
!1286 = distinct !DILexicalBlock(scope: !1287, file: !44, line: 207, column: 19)
!1287 = distinct !DILexicalBlock(scope: !1271, file: !44, line: 207, column: 9)
!1288 = !{!1289}
!1289 = !DICompositeType(tag: DW_TAG_structure_type, name: "Error", scope: !11, file: !10, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$ss5Error_pD")
!1290 = !DILocation(line: 206, column: 40, scope: !1271)
!1291 = !DILocation(line: 205, column: 73, scope: !1271)
!1292 = !DILocation(line: 207, column: 19, scope: !1286)
!1293 = !DILocation(line: 0, scope: !1287)
!1294 = !DILocation(line: 0, scope: !1271)
!1295 = !DILocation(line: 207, column: 9, scope: !1287)
!1296 = !DILocation(line: 207, column: 31, scope: !1297)
!1297 = distinct !DILexicalBlock(scope: !1287, file: !44, line: 207, column: 29)
!1298 = !DILocation(line: 0, scope: !1299)
!1299 = !DILexicalBlockFile(scope: !1286, discriminator: 0)
!1300 = !DILocation(line: 0, scope: !1286)
!1301 = !DILocation(line: 208, column: 38, scope: !1286)
!1302 = !DILocation(line: 208, column: 60, scope: !1286)
!1303 = !DILocation(line: 208, column: 9, scope: !1286)
!1304 = !DILocation(line: 209, column: 7, scope: !1286)
!1401 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_TA", scope: !2, file: !177, type: !184, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30)
!1402 = !DILocation(line: 0, scope: !1401)
!3121 = distinct !DISubprogram(linkageName: "$s16ReproKvoVideoLib24ReproVideoPlayerProviderC11setupPlayer33_DE9A0A2FE3FAF13552D292CAA2A5386FLL4itemyAA0C12PlaybackItemO_tFySo0D6LooperC_10Foundation24NSKeyValueObservedChangeVySo0dS6StatusVGtYbcfU_yyScMYcXEfU_TA", scope: !2, file: !177, type: !184, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !30)
!3122 = !DILocation(line: 0, scope: !3121)
