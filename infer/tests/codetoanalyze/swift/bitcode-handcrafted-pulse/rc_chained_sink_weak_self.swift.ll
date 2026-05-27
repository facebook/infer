; LLVM IR pinning a RETAIN_CYCLE false positive on chained
; Combine.Publisher.sink with [weak self].
;
; Repro source (schematic):
;
;   final class ChainedSinkRepro {
;     private var c1: AnyCancellable?
;     private var c2: AnyCancellable?
;     private var redirectToast: Int = 0
;     private var hasExpired = false
;
;     init(timeout: ToggleTimeout, p: AnyPublisher<Bool, Never>) {
;       c1 = timeout.expired.receive(on: DispatchQueue.main).sink { [weak self] _ in
;         guard let self else { return }
;         self.hasExpired = true
;         self.dismiss()
;         self.redirectToast = Self.helper { [weak self] in self?.dismiss() }
;       }
;       resubscribe(p)
;     }
;     private func resubscribe(_ p: AnyPublisher<Bool, Never>) {
;       c2?.cancel()
;       c2 = p.dropFirst().filter { $0 == true }.first().receive(on: DispatchQueue.main)
;         .sink { [weak self] _ in
;           guard let self, self.hasExpired else { return }
;           self.dismiss()
;         }
;     }
;     func dismiss() { redirectToast = 0 }
;     private static func helper(_ onDismiss: @escaping () -> Void) -> Int { 0 }
;   }
;
; Why the FP fires: swiftc lowers chained .receive(on:).sink { [weak self] }
; to a partial-apply forwarder whose outer holder stores `self` STRONGLY
; in field_2 (alongside the inner closure context in field_1, which carries
; the actual [weak self] slot). LLVM-15+ opaque pointers also strip the
; swift::weak element type from the inner tuple, so neither the weak slot
; nor the unrelated strong field gets Fieldname.is_weak. Pulse's cycle
; walker follows the strong edge and reports.
;
; Fix plan in notes/retain-cycle-bucket-a-investigation.md.

; ModuleID = 'infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_chained_sink_weak_self.swift'
source_filename = "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_chained_sink_weak_self.swift"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-ios17.2.0-simulator"

%swift.type_descriptor = type opaque
%objc_class = type { ptr, ptr, ptr, ptr, i64 }
%swift.protocol_conformance_descriptor = type { i32, i32, i32, i32 }
%swift.full_boxmetadata = type { ptr, ptr, %swift.type, i32, ptr }
%swift.type = type { i64 }
%swift.opaque = type opaque
%swift.method_descriptor = type { i32, i32 }
%swift.type_metadata_record = type { i32 }
%T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC = type <{ %swift.refcounted, %T7Combine14AnyCancellableCSg, %T7Combine14AnyCancellableCSg, %TSi, %TSb }>
%swift.refcounted = type { ptr, i64 }
%T7Combine14AnyCancellableCSg = type <{ [8 x i8] }>
%TSi = type <{ i64 }>
%TSb = type <{ i1 }>
%T7Combine12AnyPublisherVyyts5NeverOG = type <{ ptr }>
%swift.vwtable = type { ptr, ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64, i64, i32, i32 }
%swift.metadata_response = type { ptr, i64 }
%swift.weak = type { ptr }
%T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSg = type <{ [8 x i8] }>
%swift.function = type { ptr, ptr }
%T7Combine12AnyPublisherVySbs5NeverOG = type <{ ptr }>

@"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMn" = external global %swift.type_descriptor, align 4
@"got.$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMn" = private unnamed_addr constant ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMn"
@"symbolic _____Sg So17OS_dispatch_queueC8DispatchE16SchedulerOptionsV" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg So17OS_dispatch_queueC8DispatchE16SchedulerOptionsV", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg So17OS_dispatch_queueC8DispatchE16SchedulerOptionsV" to i64), i64 ptrtoint (ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgMD" to i64)) to i32), i32 -7 }, align 8
@"$s7Combine10PublishersO9ReceiveOnVMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine10PublishersO9ReceiveOnVMn" = private unnamed_addr constant ptr @"$s7Combine10PublishersO9ReceiveOnVMn"
@"$s7Combine12AnyPublisherVMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine12AnyPublisherVMn" = private unnamed_addr constant ptr @"$s7Combine12AnyPublisherVMn"
@"$ss5NeverOMn" = external global %swift.type_descriptor, align 4
@"got.$ss5NeverOMn" = private unnamed_addr constant ptr @"$ss5NeverOMn"
@"symbolic _____y______yyt_____GSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AA12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [24 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO9ReceiveOnVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [24 x i8], i8 }>, ptr @"symbolic _____y______yyt_____GSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AA12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [24 x i8], i8 }>, ptr @"symbolic _____y______yyt_____GSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AA12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [3 x i8] c"yyt", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [24 x i8], i8 }>, ptr @"symbolic _____y______yyt_____GSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AA12AnyPublisherV s5NeverO", i32 0, i32 7) to i64)) to i32), [24 x i8] c"GSo17OS_dispatch_queueCG", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____y______yyt_____GSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AA12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGMD" to i64)) to i32), i32 -44 }, align 8
@"$sSo17OS_dispatch_queueCML" = linkonce_odr hidden global ptr null, align 8
@"OBJC_CLASS_REF_$_OS_dispatch_queue" = private externally_initialized global ptr @"OBJC_CLASS_$_OS_dispatch_queue", section "__DATA,__objc_classrefs,regular,no_dead_strip", align 8
@"OBJC_CLASS_$_OS_dispatch_queue" = external global %objc_class, align 8
@"symbolic _____yyt_____G 7Combine12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }>, ptr @"symbolic _____yyt_____G 7Combine12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [3 x i8] c"yyt", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }>, ptr @"symbolic _____yyt_____G 7Combine12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [1 x i8] c"G", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine12AnyPublisherVyyts5NeverOGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____yyt_____G 7Combine12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine12AnyPublisherVyyts5NeverOGMD" to i64)) to i32), i32 -14 }, align 8
@"$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine12AnyPublisherVyxq_GAA0C0AAMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWL" = linkonce_odr hidden global ptr null, align 8
@"$sSo17OS_dispatch_queueC7Combine9Scheduler8DispatchMc" = external global %swift.protocol_conformance_descriptor, align 4
@"symbolic _____SgXw 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" = linkonce_odr hidden constant <{ i8, i32, [4 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____SgXw 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC", i32 0, i32 1) to i64)) to i32), [4 x i8] c"SgXw", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"\01l__swift5_reflection_descriptor" = private constant { i32, i32, i32, i32 } { i32 1, i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____SgXw 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32 }, ptr @"\01l__swift5_reflection_descriptor", i32 0, i32 3) to i64)) to i32) }, section "__TEXT,__swift5_capture, regular", no_sanitize_address, align 4
@metadata = private constant %swift.full_boxmetadata { ptr @objectdestroy, ptr null, %swift.type { i64 1024 }, i32 16, ptr @"\01l__swift5_reflection_descriptor" }, align 8
@"symbolic _____SgXwz_Xx 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" = linkonce_odr hidden constant <{ i8, i32, [8 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [8 x i8], i8 }>, ptr @"symbolic _____SgXwz_Xx 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC", i32 0, i32 1) to i64)) to i32), [8 x i8] c"SgXwz_Xx", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"symbolic _____XDXMT 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" = linkonce_odr hidden constant <{ i8, i32, [5 x i8], i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [5 x i8], i8 }>, ptr @"symbolic _____XDXMT 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC", i32 0, i32 1) to i64)) to i32), [5 x i8] c"XDXMT", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"\01l__swift5_reflection_descriptor.1" = private constant { i32, i32, i32, i32, i32 } { i32 2, i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____SgXwz_Xx 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32 }, ptr @"\01l__swift5_reflection_descriptor.1", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____XDXMT 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i32, i32, i32 }, ptr @"\01l__swift5_reflection_descriptor.1", i32 0, i32 4) to i64)) to i32) }, section "__TEXT,__swift5_capture, regular", no_sanitize_address, align 4
@metadata.3 = private constant %swift.full_boxmetadata { ptr @objectdestroy.2, ptr null, %swift.type { i64 1024 }, i32 16, ptr @"\01l__swift5_reflection_descriptor.1" }, align 8
@"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine10PublishersO9ReceiveOnVy_xq_GAA9PublisherAAMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s7Combine10PublishersO4DropVMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine10PublishersO4DropVMn" = private unnamed_addr constant ptr @"$s7Combine10PublishersO4DropVMn"
@"symbolic _____y______ySb_____GG 7Combine10PublishersO4DropV AA12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [2 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO4DropVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____y______ySb_____GG 7Combine10PublishersO4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____y______ySb_____GG 7Combine10PublishersO4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [3 x i8] c"ySb", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____y______ySb_____GG 7Combine10PublishersO4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 7) to i64)) to i32), [2 x i8] c"GG", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____y______ySb_____GG 7Combine10PublishersO4DropV AA12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGMD" to i64)) to i32), i32 -22 }, align 8
@"$s7Combine10PublishersO6FilterVMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine10PublishersO6FilterVMn" = private unnamed_addr constant ptr @"$s7Combine10PublishersO6FilterVMn"
@"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [3 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO6FilterVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [3 x i8], i8 }>, ptr @"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO4DropVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [3 x i8], i8 }>, ptr @"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [3 x i8], i8 }>, ptr @"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 7) to i64)) to i32), [3 x i8] c"ySb", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [3 x i8], i8 }>, ptr @"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 10) to i64)) to i32), [3 x i8] c"GGG", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____y______y______ySb_____GGG 7Combine10PublishersO6FilterV AC4DropV AA12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGMD" to i64)) to i32), i32 -30 }, align 8
@"$s7Combine10PublishersO5FirstVMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine10PublishersO5FirstVMn" = private unnamed_addr constant ptr @"$s7Combine10PublishersO5FirstVMn"
@"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO5FirstVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO6FilterVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO4DropVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 7) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 10) to i64)) to i32), [3 x i8] c"ySb", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [4 x i8], i8 }>, ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 13) to i64)) to i32), [4 x i8] c"GGGG", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____y______y______y______ySb_____GGGG 7Combine10PublishersO5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGMD" to i64)) to i32), i32 -38 }, align 8
@"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO9ReceiveOnVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO5FirstVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO6FilterVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 7) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine10PublishersO4DropVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 10) to i64)) to i32), [2 x i8] c"y_", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 13) to i64)) to i32), [3 x i8] c"ySb", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [2 x i8], i8, i32, [3 x i8], i8, i32, [27 x i8], i8 }>, ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO", i32 0, i32 16) to i64)) to i32), [27 x i8] c"GGGGSo17OS_dispatch_queueCG", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____y______y______y______y______ySb_____GGGGSo17OS_dispatch_queueCG 7Combine10PublishersO9ReceiveOnV AC5FirstV AC6FilterV AC4DropV AA12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGMD" to i64)) to i32), i32 -68 }, align 8
@"symbolic _____ySb_____G 7Combine12AnyPublisherV s5NeverO" = linkonce_odr hidden constant <{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine12AnyPublisherVMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }>, ptr @"symbolic _____ySb_____G 7Combine12AnyPublisherV s5NeverO", i32 0, i32 1) to i64)) to i32), [3 x i8] c"ySb", i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$ss5NeverOMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [3 x i8], i8, i32, [1 x i8], i8 }>, ptr @"symbolic _____ySb_____G 7Combine12AnyPublisherV s5NeverO", i32 0, i32 4) to i64)) to i32), [1 x i8] c"G", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine12AnyPublisherVySbs5NeverOGMD" = linkonce_odr hidden global { i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ySb_____G 7Combine12AnyPublisherV s5NeverO" to i64), i64 ptrtoint (ptr @"$s7Combine12AnyPublisherVySbs5NeverOGMD" to i64)) to i32), i32 -14 }, align 8
@"$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine10PublishersO4DropVy_xGAA9PublisherAAMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine10PublishersO6FilterVy_xGAA9PublisherAAMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s7Combine10PublishersO5FirstVy_xGAA9PublisherAAMc" = external global %swift.protocol_conformance_descriptor, align 4
@"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWL" = linkonce_odr hidden global ptr null, align 8
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC2c133_93366EAB6BF230CF2051527E7A0D12D2LL7Combine14AnyCancellableCSgvpWvd" = internal constant i64 16, align 8
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC2c233_93366EAB6BF230CF2051527E7A0D12D2LL7Combine14AnyCancellableCSgvpWvd" = internal constant i64 24, align 8
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC13redirectToast33_93366EAB6BF230CF2051527E7A0D12D2LLSivpWvd" = internal constant i64 32, align 8
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC10hasExpired33_93366EAB6BF230CF2051527E7A0D12D2LLSbvpWvd" = internal constant i64 40, align 8
@"$sBoWV" = external global ptr, align 8
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMm" = hidden global %objc_class { ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @"OBJC_METACLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 ptrtoint (ptr @_METACLASS_DATA__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro to i64) }, align 8
@"OBJC_CLASS_$__TtCs12_SwiftObject" = external global %objc_class, align 8
@_objc_empty_cache = external global %swift.opaque
@"OBJC_METACLASS_$__TtCs12_SwiftObject" = external global %objc_class, align 8
@.str.57._TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro = private unnamed_addr constant [58 x i8] c"_TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro\00", section "__TEXT,__objc_classname,cstring_literals"
@_METACLASS_DATA__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 129, i32 40, i32 40, i32 0, ptr null, ptr @.str.57._TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro, ptr null, ptr null, ptr null, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.2.c1 = private unnamed_addr constant [3 x i8] c"c1\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.0. = private unnamed_addr constant [1 x i8] zeroinitializer, section "__TEXT,__objc_methtype,cstring_literals"
@.str.2.c2 = private unnamed_addr constant [3 x i8] c"c2\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.13.redirectToast = private unnamed_addr constant [14 x i8] c"redirectToast\00", section "__TEXT,__objc_methname,cstring_literals"
@.str.10.hasExpired = private unnamed_addr constant [11 x i8] c"hasExpired\00", section "__TEXT,__objc_methname,cstring_literals"
@_IVARS__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro = internal constant { i32, i32, [4 x { ptr, ptr, ptr, i32, i32 }] } { i32 32, i32 4, [4 x { ptr, ptr, ptr, i32, i32 }] [{ ptr, ptr, ptr, i32, i32 } { ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC2c133_93366EAB6BF230CF2051527E7A0D12D2LL7Combine14AnyCancellableCSgvpWvd", ptr @.str.2.c1, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC2c233_93366EAB6BF230CF2051527E7A0D12D2LL7Combine14AnyCancellableCSgvpWvd", ptr @.str.2.c2, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC13redirectToast33_93366EAB6BF230CF2051527E7A0D12D2LLSivpWvd", ptr @.str.13.redirectToast, ptr @.str.0., i32 3, i32 8 }, { ptr, ptr, ptr, i32, i32 } { ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC10hasExpired33_93366EAB6BF230CF2051527E7A0D12D2LLSbvpWvd", ptr @.str.10.hasExpired, ptr @.str.0., i32 0, i32 1 }] }, section "__DATA, __objc_const", align 8
@_DATA__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro = internal constant { i32, i32, i32, i32, ptr, ptr, ptr, ptr, ptr, ptr, ptr } { i32 128, i32 16, i32 41, i32 0, ptr null, ptr @.str.57._TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro, ptr null, ptr null, ptr @_IVARS__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro, ptr null, ptr null }, section "__DATA, __objc_const", align 8
@.str.33.RcChainedSinkReproSwiftBitcodeFix = private constant [34 x i8] c"RcChainedSinkReproSwiftBitcodeFix\00"
@"$s33RcChainedSinkReproSwiftBitcodeFixMXM" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.33.RcChainedSinkReproSwiftBitcodeFix to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32 }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFixMXM", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__constg_swiftt", align 4
@.str.16.ChainedSinkRepro = private constant [17 x i8] c"ChainedSinkRepro\00"
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn" = hidden constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }> <{ i32 -2147483568, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFixMXM" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @.str.16.ChainedSinkRepro to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMa" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", i32 0, i32 4) to i64)) to i32), i32 0, i32 3, i32 15, i32 5, i32 4, i32 10, i32 14, i32 1, %swift.method_descriptor { i32 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfC" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, %swift.method_descriptor }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", i32 0, i32 13, i32 1) to i64)) to i32) } }>, section "__TEXT,__constg_swiftt", align 4
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMf" = internal global <{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, i64, i64, ptr }> <{ ptr null, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfD", ptr @"$sBoWV", i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMm" to i64), ptr @"OBJC_CLASS_$__TtCs12_SwiftObject", ptr @_objc_empty_cache, ptr null, i64 add (i64 ptrtoint (ptr @_DATA__TtC33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkRepro to i64), i64 2), i32 2, i32 0, i32 41, i16 7, i16 0, i32 144, i32 24, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn", ptr null, i64 16, i64 24, i64 32, i64 40, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfC" }>, align 8
@"symbolic _____ 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, i8 }>, ptr @"symbolic _____ 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@"$s7Combine14AnyCancellableCMn" = external global %swift.type_descriptor, align 4
@"got.$s7Combine14AnyCancellableCMn" = private unnamed_addr constant ptr @"$s7Combine14AnyCancellableCMn"
@"symbolic _____Sg 7Combine14AnyCancellableC" = linkonce_odr hidden constant <{ i8, i32, [2 x i8], i8 }> <{ i8 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"got.$s7Combine14AnyCancellableCMn" to i64), i64 ptrtoint (ptr getelementptr inbounds (<{ i8, i32, [2 x i8], i8 }>, ptr @"symbolic _____Sg 7Combine14AnyCancellableC", i32 0, i32 1) to i64)) to i32), [2 x i8] c"Sg", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@0 = private constant [3 x i8] c"c1\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@1 = private constant [3 x i8] c"c2\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic Si" = linkonce_odr hidden constant <{ [2 x i8], i8 }> <{ [2 x i8] c"Si", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@2 = private constant [14 x i8] c"redirectToast\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"symbolic Sb" = linkonce_odr hidden constant <{ [2 x i8], i8 }> <{ [2 x i8] c"Sb", i8 0 }>, section "__TEXT,__swift5_typeref, regular", no_sanitize_address, align 2
@3 = private constant [11 x i8] c"hasExpired\00", section "__TEXT,__swift5_reflstr, regular", no_sanitize_address
@"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF" = internal constant { i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 } { i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____ 33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC" to i64), i64 ptrtoint (ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF" to i64)) to i32), i32 0, i16 1, i16 12, i32 4, i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 7Combine14AnyCancellableC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 6) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @0 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 7) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic _____Sg 7Combine14AnyCancellableC" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 9) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @1 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 10) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic Si" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 12) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @2 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 13) to i64)) to i32), i32 2, i32 trunc (i64 sub (i64 ptrtoint (ptr @"symbolic Sb" to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 15) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (ptr @3 to i64), i64 ptrtoint (ptr getelementptr inbounds ({ i32, i32, i16, i16, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMF", i32 0, i32 16) to i64)) to i32) }, section "__TEXT,__swift5_fieldmd, regular", no_sanitize_address, align 4
@".str.48.RcChainedSinkReproSwiftBitcodeFix/tth_min5.swift" = private unnamed_addr constant [49 x i8] c"RcChainedSinkReproSwiftBitcodeFix/tth_min5.swift\00"
@".str.15.resubscribe(_:)" = private unnamed_addr constant [16 x i8] c"resubscribe(_:)\00"
@.str.1.x = private unnamed_addr constant [2 x i8] c"x\00"



; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #1

; Function Attrs: nounwind
declare void @swift_beginAccess(ptr, ptr, i64, ptr) #2

; Function Attrs: nounwind willreturn
declare ptr @swift_retain(ptr returned) #3

; Function Attrs: nounwind
declare void @swift_endAccess(ptr) #2

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #1


; Function Attrs: nounwind
declare void @swift_release(ptr) #2







define hidden swiftcc ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfC"(ptr %0, ptr %1, ptr swiftself %2) #0 !dbg !172 {
entry:
  %3 = call noalias ptr @swift_allocObject(ptr %2, i64 41, i64 7) #2, !dbg !180
  %4 = call swiftcc ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfc"(ptr %0, ptr %1, ptr swiftself %3), !dbg !180
  ret ptr %4, !dbg !180
}

; Function Attrs: nounwind
declare ptr @swift_allocObject(ptr, i64, i64) #2

define hidden swiftcc ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfc"(ptr %0, ptr %1, ptr swiftself %2) #0 !dbg !181 {
entry:
  %3 = load ptr, ptr %2, align 8
  %timeout.debug = alloca ptr, align 8
    #dbg_declare(ptr %timeout.debug, !186, !DIExpression(), !200)
  call void @llvm.memset.p0.i64(ptr align 8 %timeout.debug, i8 0, i64 8, i1 false)
  %p.debug = alloca ptr, align 8
    #dbg_declare(ptr %p.debug, !188, !DIExpression(), !201)
  call void @llvm.memset.p0.i64(ptr align 8 %p.debug, i8 0, i64 8, i1 false)
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !198, !DIExpression(), !202)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %4 = alloca %T7Combine12AnyPublisherVyyts5NeverOG, align 8
  %5 = alloca ptr, align 8
  %access-scratch = alloca [24 x i8], align 8
  %6 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgMD") #10, !dbg !203
  %7 = getelementptr inbounds ptr, ptr %6, i64 -1, !dbg !203
  %.valueWitnesses = load ptr, ptr %7, align 8, !dbg !203, !invariant.load !205, !dereferenceable !206
  %8 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses, i32 0, i32 8, !dbg !203
  %size = load i64, ptr %8, align 8, !dbg !203, !invariant.load !205
  %9 = alloca i8, i64 %size, align 16, !dbg !203
  call void @llvm.lifetime.start.p0(i64 -1, ptr %9), !dbg !203
  %10 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGMD") #10, !dbg !203
  %11 = getelementptr inbounds ptr, ptr %10, i64 -1, !dbg !203
  %.valueWitnesses1 = load ptr, ptr %11, align 8, !dbg !203, !invariant.load !205, !dereferenceable !206
  %12 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses1, i32 0, i32 8, !dbg !203
  %size2 = load i64, ptr %12, align 8, !dbg !203, !invariant.load !205
  %13 = alloca i8, i64 %size2, align 16, !dbg !203
  call void @llvm.lifetime.start.p0(i64 -1, ptr %13), !dbg !203
  store ptr %0, ptr %timeout.debug, align 8, !dbg !207
  store ptr %1, ptr %p.debug, align 8, !dbg !207
  store ptr %2, ptr %self.debug, align 8, !dbg !207
  %14 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %2, i32 0, i32 1, !dbg !208
  store i64 0, ptr %14, align 8, !dbg !212
  %15 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %2, i32 0, i32 2, !dbg !214
  store i64 0, ptr %15, align 8, !dbg !216
  %16 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %2, i32 0, i32 3, !dbg !218
  %._value = getelementptr inbounds nuw %TSi, ptr %16, i32 0, i32 0, !dbg !220
  store i64 0, ptr %._value, align 8, !dbg !220
  %17 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %2, i32 0, i32 4, !dbg !222
  %._value3 = getelementptr inbounds nuw %TSb, ptr %17, i32 0, i32 0, !dbg !224
  store i8 0, ptr %._value3, align 8, !dbg !224
  %18 = call swiftcc ptr @"$s39FakeFrameworkForChainedSinkBitcodeTest00bcD13ToggleTimeoutC7expired7Combine12AnyPublisherVyyts5NeverOGvg"(ptr swiftself %0), !dbg !226
  call void @llvm.lifetime.start.p0(i64 8, ptr %4), !dbg !203
  %.box = getelementptr inbounds nuw %T7Combine12AnyPublisherVyyts5NeverOG, ptr %4, i32 0, i32 0, !dbg !226
  store ptr %18, ptr %.box, align 8, !dbg !226
  %19 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueCMa"(i64 0) #12, !dbg !227
  %20 = extractvalue %swift.metadata_response %19, 0, !dbg !227
  %21 = call swiftcc ptr @"$sSo17OS_dispatch_queueC8DispatchE4mainABvgZ"(ptr swiftself %20), !dbg !228
  call void @llvm.lifetime.start.p0(i64 8, ptr %5), !dbg !203
  store ptr %21, ptr %5, align 8, !dbg !228
  %22 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMa"(i64 0) #12, !dbg !207
  %23 = extractvalue %swift.metadata_response %22, 0, !dbg !207
  %24 = getelementptr inbounds ptr, ptr %23, i64 -1, !dbg !207
  %.valueWitnesses4 = load ptr, ptr %24, align 8, !dbg !207, !invariant.load !205, !dereferenceable !206
  %25 = getelementptr inbounds ptr, ptr %.valueWitnesses4, i32 7, !dbg !207
  %StoreEnumTagSinglePayload = load ptr, ptr %25, align 8, !dbg !207, !invariant.load !205
  call void %StoreEnumTagSinglePayload(ptr noalias %9, i32 1, i32 1, ptr %23) #2, !dbg !207
  %26 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine12AnyPublisherVyyts5NeverOGMD") #10, !dbg !229
  %27 = call ptr @"$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWl"() #12, !dbg !229
  %28 = call ptr @"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWl"() #12, !dbg !229
  call swiftcc void @"$s7Combine9PublisherPAAE7receive2on7optionsAA10PublishersO9ReceiveOnVy_xqd__Gqd___16SchedulerOptionsQyd__SgtAA0I0Rd__lF"(ptr noalias sret(%swift.opaque) %13, ptr noalias %5, ptr noalias %9, ptr %26, ptr %20, ptr %27, ptr %28, ptr noalias swiftself %4), !dbg !229
  %29 = call ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgWOh"(ptr %9), !dbg !230
  %toDestroy = load ptr, ptr %5, align 8, !dbg !230
  call void @llvm.objc.release(ptr %toDestroy), !dbg !230
  call void @llvm.lifetime.end.p0(i64 8, ptr %5), !dbg !230
  %30 = call ptr @"$s7Combine12AnyPublisherVyyts5NeverOGWOh"(ptr %4), !dbg !230
  call void @llvm.lifetime.end.p0(i64 8, ptr %4), !dbg !230
  %31 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata, i32 0, i32 2), i64 24, i64 7) #2, !dbg !232
  %32 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %31, i32 0, i32 1, !dbg !232
  %33 = call ptr @swift_retain(ptr returned %2) #3, !dbg !235
  %34 = ptrtoint ptr %2 to i64, !dbg !207
  %35 = inttoptr i64 %34 to ptr, !dbg !207
  %36 = call ptr @swift_weakInit(ptr returned %32, ptr %35) #2, !dbg !207
  %37 = inttoptr i64 %34 to ptr, !dbg !207
  call void @swift_release(ptr %37) #2, !dbg !207
  %38 = call ptr @swift_retain(ptr returned %31) #3, !dbg !207
  %39 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata.3, i32 0, i32 2), i64 32, i64 7) #2, !dbg !236
  %40 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %39, i32 0, i32 1, !dbg !236
  store ptr %31, ptr %40, align 8, !dbg !236
  %41 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %39, i32 0, i32 2, !dbg !236
  store ptr %3, ptr %41, align 8, !dbg !236
  call void @swift_release(ptr %31) #2, !dbg !230
  %42 = call ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWl"() #12, !dbg !237
  %43 = call swiftcc ptr @"$s7Combine9PublisherPAAs5NeverO7FailureRtzrlE4sink12receiveValueAA14AnyCancellableCy6OutputQzc_tF"(ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_TA", ptr %39, ptr %10, ptr %42, ptr noalias swiftself %13), !dbg !237
  call void @swift_release(ptr %39) #2, !dbg !230
  %44 = getelementptr inbounds ptr, ptr %.valueWitnesses1, i32 1, !dbg !230
  %Destroy = load ptr, ptr %44, align 8, !dbg !230, !invariant.load !205
  call void %Destroy(ptr noalias %13, ptr %10) #2, !dbg !230
  %45 = ptrtoint ptr %43 to i64, !dbg !207
  %46 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %2, i32 0, i32 1, !dbg !238
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !238
  call void @swift_beginAccess(ptr %46, ptr %access-scratch, i64 33, ptr null) #2, !dbg !238
  %47 = load i64, ptr %46, align 8, !dbg !238
  store i64 %45, ptr %46, align 8, !dbg !238
  %48 = inttoptr i64 %47 to ptr, !dbg !238
  call void @swift_release(ptr %48) #2, !dbg !238
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !207
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !207
  call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGF"(ptr %1, ptr swiftself %2), !dbg !239
  call void @swift_release(ptr %1) #2, !dbg !240
  call void @swift_release(ptr %0) #2, !dbg !240
  call void @llvm.lifetime.end.p0(i64 -1, ptr %13), !dbg !240
  call void @llvm.lifetime.end.p0(i64 -1, ptr %9), !dbg !240
  ret ptr %2, !dbg !240
}

; Function Attrs: noinline nounwind willreturn memory(read)
define linkonce_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledName(ptr %0) #4 !dbg !241 {
entry:
  %1 = load atomic i64, ptr %0 monotonic, align 8, !dbg !243
  %2 = icmp slt i64 %1, 0, !dbg !243
  %3 = call i1 @llvm.expect.i1(i1 %2, i1 false), !dbg !243
  br i1 %3, label %7, label %4, !dbg !243

4:                                                ; preds = %7, %entry
  %5 = phi i64 [ %1, %entry ], [ %16, %7 ], !dbg !243
  %6 = inttoptr i64 %5 to ptr, !dbg !243
  ret ptr %6, !dbg !243

7:                                                ; preds = %entry
  %8 = ashr i64 %1, 32, !dbg !243
  %9 = sub i64 0, %8, !dbg !243
  %10 = trunc i64 %1 to i32, !dbg !243
  %11 = sext i32 %10 to i64, !dbg !243
  %12 = ptrtoint ptr %0 to i64, !dbg !243
  %13 = add i64 %12, %11, !dbg !243
  %14 = inttoptr i64 %13 to ptr, !dbg !243
  %15 = call swiftcc ptr @swift_getTypeByMangledNameInContext2(ptr %14, i64 %9, ptr null, ptr null) #13, !dbg !243
  %16 = ptrtoint ptr %15 to i64, !dbg !243
  store atomic i64 %16, ptr %0 monotonic, align 8, !dbg !243
  br label %4, !dbg !243
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare i1 @llvm.expect.i1(i1, i1) #5

; Function Attrs: nounwind memory(argmem: readwrite)
declare swiftcc ptr @swift_getTypeByMangledNameInContext2(ptr, i64, ptr, ptr) #6

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #7

declare swiftcc ptr @"$s39FakeFrameworkForChainedSinkBitcodeTest00bcD13ToggleTimeoutC7expired7Combine12AnyPublisherVyyts5NeverOGvg"(ptr swiftself) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueCMa"(i64 %0) #8 !dbg !244 {
entry:
  %1 = load ptr, ptr @"$sSo17OS_dispatch_queueCML", align 8, !dbg !245
  %2 = icmp eq ptr %1, null, !dbg !245
  br i1 %2, label %cacheIsNull, label %cont, !dbg !245

cacheIsNull:                                      ; preds = %entry
  %3 = load ptr, ptr @"OBJC_CLASS_REF_$_OS_dispatch_queue", align 8, !dbg !245
  %4 = call ptr @objc_opt_self(ptr %3) #2, !dbg !245
  %5 = call ptr @swift_getObjCClassMetadata(ptr %4) #9, !dbg !245
  store atomic ptr %5, ptr @"$sSo17OS_dispatch_queueCML" release, align 8, !dbg !245
  br label %cont, !dbg !245

cont:                                             ; preds = %cacheIsNull, %entry
  %6 = phi ptr [ %1, %entry ], [ %5, %cacheIsNull ], !dbg !245
  %7 = insertvalue %swift.metadata_response undef, ptr %6, 0, !dbg !245
  %8 = insertvalue %swift.metadata_response %7, i64 0, 1, !dbg !245
  ret %swift.metadata_response %8, !dbg !245
}

; Function Attrs: nounwind
declare ptr @objc_opt_self(ptr) #2

; Function Attrs: nounwind willreturn memory(none)
declare ptr @swift_getObjCClassMetadata(ptr) #9

declare swiftcc ptr @"$sSo17OS_dispatch_queueC8DispatchE4mainABvgZ"(ptr swiftself) #0

declare swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMa"(i64) #0

declare swiftcc void @"$s7Combine9PublisherPAAE7receive2on7optionsAA10PublishersO9ReceiveOnVy_xqd__Gqd___16SchedulerOptionsQyd__SgtAA0I0Rd__lF"(ptr noalias sret(%swift.opaque), ptr noalias, ptr noalias, ptr, ptr, ptr, ptr, ptr noalias swiftself) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWl"() #8 !dbg !246 {
entry:
  %0 = load ptr, ptr @"$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWL", align 8, !dbg !247
  %1 = icmp eq ptr %0, null, !dbg !247
  br i1 %1, label %cacheIsNull, label %cont, !dbg !247

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine12AnyPublisherVyyts5NeverOGMD") #10, !dbg !247
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine12AnyPublisherVyxq_GAA0C0AAMc", ptr %2, ptr undef) #10, !dbg !247
  store atomic ptr %3, ptr @"$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWL" release, align 8, !dbg !247
  br label %cont, !dbg !247

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !247
  ret ptr %4, !dbg !247
}

; Function Attrs: noinline nounwind willreturn memory(read)
define linkonce_odr hidden ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr %0) #4 !dbg !248 {
entry:
  %1 = load atomic i64, ptr %0 monotonic, align 8, !dbg !249
  %2 = icmp slt i64 %1, 0, !dbg !249
  %3 = call i1 @llvm.expect.i1(i1 %2, i1 false), !dbg !249
  br i1 %3, label %7, label %4, !dbg !249

4:                                                ; preds = %7, %entry
  %5 = phi i64 [ %1, %entry ], [ %16, %7 ], !dbg !249
  %6 = inttoptr i64 %5 to ptr, !dbg !249
  ret ptr %6, !dbg !249

7:                                                ; preds = %entry
  %8 = ashr i64 %1, 32, !dbg !249
  %9 = sub i64 0, %8, !dbg !249
  %10 = trunc i64 %1 to i32, !dbg !249
  %11 = sext i32 %10 to i64, !dbg !249
  %12 = ptrtoint ptr %0 to i64, !dbg !249
  %13 = add i64 %12, %11, !dbg !249
  %14 = inttoptr i64 %13 to ptr, !dbg !249
  %15 = call swiftcc ptr @swift_getTypeByMangledNameInContextInMetadataState2(i64 255, ptr %14, i64 %9, ptr null, ptr null) #13, !dbg !249
  %16 = ptrtoint ptr %15 to i64, !dbg !249
  store atomic i64 %16, ptr %0 monotonic, align 8, !dbg !249
  br label %4, !dbg !249
}

; Function Attrs: nounwind memory(argmem: readwrite)
declare swiftcc ptr @swift_getTypeByMangledNameInContextInMetadataState2(i64, ptr, i64, ptr, ptr) #6

; Function Attrs: nounwind memory(read)
declare ptr @swift_getWitnessTable(ptr, ptr, ptr) #10

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWl"() #8 !dbg !250 {
entry:
  %0 = load ptr, ptr @"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWL", align 8, !dbg !251
  %1 = icmp eq ptr %0, null, !dbg !251
  br i1 %1, label %cacheIsNull, label %cont, !dbg !251

cacheIsNull:                                      ; preds = %entry
  %2 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueCMa"(i64 255) #12, !dbg !251
  %3 = extractvalue %swift.metadata_response %2, 0, !dbg !251
  %4 = extractvalue %swift.metadata_response %2, 1, !dbg !251
  %5 = call ptr @swift_getWitnessTable(ptr @"$sSo17OS_dispatch_queueC7Combine9Scheduler8DispatchMc", ptr %3, ptr undef) #10, !dbg !251
  store atomic ptr %5, ptr @"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWL" release, align 8, !dbg !251
  br label %cont, !dbg !251

cont:                                             ; preds = %cacheIsNull, %entry
  %6 = phi ptr [ %0, %entry ], [ %5, %cacheIsNull ], !dbg !251
  ret ptr %6, !dbg !251
}

; Function Attrs: noinline nounwind
define linkonce_odr hidden ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgWOh"(ptr %0) #11 !dbg !252 {
entry:
  %1 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMa"(i64 0) #12, !dbg !253
  %2 = extractvalue %swift.metadata_response %1, 0, !dbg !253
  %3 = getelementptr inbounds ptr, ptr %2, i64 -1, !dbg !253
  %.valueWitnesses = load ptr, ptr %3, align 8, !dbg !253, !invariant.load !205, !dereferenceable !206
  %4 = getelementptr inbounds ptr, ptr %.valueWitnesses, i32 6, !dbg !253
  %GetEnumTagSinglePayload = load ptr, ptr %4, align 8, !dbg !253, !invariant.load !205
  %5 = call i32 %GetEnumTagSinglePayload(ptr noalias %0, i32 1, ptr %2) #10, !dbg !253
  %6 = icmp eq i32 %5, 0, !dbg !253
  br i1 %6, label %7, label %9, !dbg !253

7:                                                ; preds = %entry
  %8 = getelementptr inbounds ptr, ptr %.valueWitnesses, i32 1, !dbg !253
  %Destroy = load ptr, ptr %8, align 8, !dbg !253, !invariant.load !205
  call void %Destroy(ptr noalias %0, ptr %2) #2, !dbg !253
  br label %9, !dbg !253

9:                                                ; preds = %7, %entry
  ret ptr %0, !dbg !253
}

; Function Attrs: nounwind
declare void @llvm.objc.release(ptr) #2

; Function Attrs: noinline nounwind
define linkonce_odr hidden ptr @"$s7Combine12AnyPublisherVyyts5NeverOGWOh"(ptr %0) #11 !dbg !254 {
entry:
  %.box = getelementptr inbounds nuw %T7Combine12AnyPublisherVyyts5NeverOG, ptr %0, i32 0, i32 0, !dbg !255
  %toDestroy = load ptr, ptr %.box, align 8, !dbg !255
  call void @swift_release(ptr %toDestroy) #2, !dbg !255
  ret ptr %0, !dbg !255
}

define private swiftcc void @objectdestroy(ptr swiftself %0) #0 !dbg !256 {
entry:
  %1 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %0, i32 0, i32 1, !dbg !257
  call void @swift_weakDestroy(ptr %1) #2, !dbg !257
  call void @swift_deallocObject(ptr %0, i64 24, i64 7) #2, !dbg !257
  ret void, !dbg !257
}

; Function Attrs: nounwind
declare void @swift_weakDestroy(ptr) #2

; Function Attrs: nounwind
declare void @swift_deallocObject(ptr, i64, i64) #2

; Function Attrs: nounwind willreturn
declare ptr @swift_weakInit(ptr returned, ptr) #3

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_"(ptr noalias captures(none) %0, ptr %1, ptr %2) #0 !dbg !258 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !267, !DIExpression(DW_OP_deref), !278)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %access-scratch = alloca [24 x i8], align 8
  %self.debug1 = alloca ptr, align 8
    #dbg_declare(ptr %self.debug1, !275, !DIExpression(), !279)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug1, i8 0, i64 8, i1 false)
  %access-scratch2 = alloca [24 x i8], align 8
  %access-scratch3 = alloca [24 x i8], align 8
    #dbg_value(i64 0, !265, !DIExpression(), !280)
  %3 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %1, i32 0, i32 1, !dbg !278
  store ptr %3, ptr %self.debug, align 8, !dbg !281
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !282
  call void @swift_beginAccess(ptr %3, ptr %access-scratch, i64 32, ptr null) #2, !dbg !282
  %4 = call ptr @swift_weakLoadStrong(ptr %3) #2, !dbg !282
  %5 = ptrtoint ptr %4 to i64, !dbg !282
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !281
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !281
  %6 = icmp eq i64 %5, 0, !dbg !282
  br i1 %6, label %9, label %7, !dbg !282

7:                                                ; preds = %entry
  %8 = inttoptr i64 %5 to ptr, !dbg !282
  br label %10, !dbg !282

9:                                                ; preds = %entry
  br label %23, !dbg !283

10:                                               ; preds = %7
  %11 = phi ptr [ %8, %7 ], !dbg !285
  store ptr %11, ptr %self.debug1, align 8, !dbg !287
  %12 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %11, i32 0, i32 4, !dbg !288
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch2), !dbg !288
  call void @swift_beginAccess(ptr %12, ptr %access-scratch2, i64 33, ptr null) #2, !dbg !288
  %._value = getelementptr inbounds nuw %TSb, ptr %12, i32 0, i32 0, !dbg !288
  store i8 1, ptr %._value, align 8, !dbg !288
  call void @swift_endAccess(ptr %access-scratch2) #2, !dbg !287
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch2), !dbg !287
  call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF"(ptr swiftself %11), !dbg !289
  %13 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata, i32 0, i32 2), i64 24, i64 7) #2, !dbg !290
  %14 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %13, i32 0, i32 1, !dbg !290
  %15 = call ptr @swift_retain(ptr returned %11) #3, !dbg !293
  %16 = ptrtoint ptr %11 to i64, !dbg !287
  %17 = inttoptr i64 %16 to ptr, !dbg !287
  %18 = call ptr @swift_weakInit(ptr returned %14, ptr %17) #2, !dbg !287
  %19 = inttoptr i64 %16 to ptr, !dbg !287
  call void @swift_release(ptr %19) #2, !dbg !287
  %20 = call ptr @swift_retain(ptr returned %13) #3, !dbg !287
  %21 = call swiftcc i64 @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC6helper33_93366EAB6BF230CF2051527E7A0D12D2LLySiyycFZ"(ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_TA", ptr %13, ptr swiftself %2), !dbg !294
  call void @swift_release(ptr %13) #2, !dbg !295
  call void @swift_release(ptr %13) #2, !dbg !295
  %22 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %11, i32 0, i32 3, !dbg !297
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch3), !dbg !297
  call void @swift_beginAccess(ptr %22, ptr %access-scratch3, i64 33, ptr null) #2, !dbg !297
  %._value4 = getelementptr inbounds nuw %TSi, ptr %22, i32 0, i32 0, !dbg !297
  store i64 %21, ptr %._value4, align 8, !dbg !297
  call void @swift_endAccess(ptr %access-scratch3) #2, !dbg !287
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch3), !dbg !287
  call void @swift_release(ptr %11) #2, !dbg !298
  br label %23, !dbg !298

23:                                               ; preds = %10, %9
  ret void, !dbg !298
}

define private swiftcc void @objectdestroy.2(ptr swiftself %0) #0 !dbg !299 {
entry:
  %1 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %0, i32 0, i32 1, !dbg !300
  %toDestroy = load ptr, ptr %1, align 8, !dbg !300
  call void @swift_release(ptr %toDestroy) #2, !dbg !300
  call void @swift_deallocObject(ptr %0, i64 32, i64 7) #2, !dbg !300
  ret void, !dbg !300
}

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_TA"(ptr noalias captures(none) %0, ptr swiftself %1) #0 !dbg !301 {
entry:
  %2 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %1, i32 0, i32 1, !dbg !302
  %3 = load ptr, ptr %2, align 8, !dbg !302
  %4 = getelementptr inbounds nuw <{ %swift.refcounted, ptr, ptr }>, ptr %1, i32 0, i32 2, !dbg !302
  %5 = load ptr, ptr %4, align 8, !dbg !302
  tail call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_"(ptr noalias captures(none) %0, ptr %3, ptr %5), !dbg !302
  ret void, !dbg !302
}

declare swiftcc ptr @"$s7Combine9PublisherPAAs5NeverO7FailureRtzrlE4sink12receiveValueAA14AnyCancellableCy6OutputQzc_tF"(ptr, ptr, ptr, ptr, ptr noalias swiftself) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWl"() #8 !dbg !303 {
entry:
  %0 = load ptr, ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWL", align 8, !dbg !304
  %1 = icmp eq ptr %0, null, !dbg !304
  br i1 %1, label %cacheIsNull, label %cont, !dbg !304

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGMD") #10, !dbg !304
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine10PublishersO9ReceiveOnVy_xq_GAA9PublisherAAMc", ptr %2, ptr undef) #10, !dbg !304
  store atomic ptr %3, ptr @"$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWL" release, align 8, !dbg !304
  br label %cont, !dbg !304

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !304
  ret ptr %4, !dbg !304
}

define hidden swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF"(ptr swiftself %0) #0 !dbg !305 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !310, !DIExpression(), !311)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %access-scratch = alloca [24 x i8], align 8
  store ptr %0, ptr %self.debug, align 8, !dbg !312
  %1 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %0, i32 0, i32 3, !dbg !313
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !313
  call void @swift_beginAccess(ptr %1, ptr %access-scratch, i64 33, ptr null) #2, !dbg !313
  %._value = getelementptr inbounds nuw %TSi, ptr %1, i32 0, i32 0, !dbg !313
  store i64 0, ptr %._value, align 8, !dbg !313
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !312
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !312
  ret void, !dbg !314
}

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_"(ptr %0) #0 !dbg !315 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !320, !DIExpression(DW_OP_deref), !321)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %access-scratch = alloca [24 x i8], align 8
  %1 = alloca %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSg, align 8
  %2 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %0, i32 0, i32 1, !dbg !321
  store ptr %2, ptr %self.debug, align 8, !dbg !322
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !324
  call void @swift_beginAccess(ptr %2, ptr %access-scratch, i64 32, ptr null) #2, !dbg !324
  call void @llvm.lifetime.start.p0(i64 8, ptr %1), !dbg !325
  %3 = call ptr @swift_weakLoadStrong(ptr %2) #2, !dbg !324
  %4 = ptrtoint ptr %3 to i64, !dbg !324
  store i64 %4, ptr %1, align 8, !dbg !324
  %5 = load i64, ptr %1, align 8, !dbg !324
  %6 = icmp eq i64 %5, 0, !dbg !324
  %7 = xor i1 %6, true, !dbg !324
  br i1 %7, label %10, label %8, !dbg !324

8:                                                ; preds = %entry
  %9 = call ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgWOh"(ptr %1), !dbg !327
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !327
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !322
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !322
  br label %14, !dbg !322

10:                                               ; preds = %entry
  %11 = load ptr, ptr %1, align 8, !dbg !324
  %12 = call ptr @swift_retain(ptr returned %11) #3, !dbg !324
  %13 = call ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgWOh"(ptr %1), !dbg !327
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !322
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !322
  call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF"(ptr swiftself %11), !dbg !329
  call void @swift_release(ptr %11) #2, !dbg !327
  call void @llvm.lifetime.end.p0(i64 8, ptr %1), !dbg !327
  br label %14, !dbg !322

14:                                               ; preds = %10, %8
  %15 = phi i8 [ 1, %8 ], [ 0, %10 ], !dbg !330
  ret void, !dbg !330
}

define internal swiftcc i64 @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC6helper33_93366EAB6BF230CF2051527E7A0D12D2LLySiyycFZ"(ptr %0, ptr %1, ptr swiftself %2) #0 !dbg !331 {
entry:
  %onDismiss.debug = alloca %swift.function, align 8
    #dbg_declare(ptr %onDismiss.debug, !337, !DIExpression(), !341)
  call void @llvm.memset.p0.i64(ptr align 8 %onDismiss.debug, i8 0, i64 16, i1 false)
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !339, !DIExpression(), !342)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  call void @llvm.lifetime.start.p0(i64 16, ptr %onDismiss.debug), !dbg !343
  %onDismiss.debug.fn = getelementptr inbounds nuw %swift.function, ptr %onDismiss.debug, i32 0, i32 0, !dbg !345
  store ptr %0, ptr %onDismiss.debug.fn, align 8, !dbg !345
  %onDismiss.debug.data = getelementptr inbounds nuw %swift.function, ptr %onDismiss.debug, i32 0, i32 1, !dbg !345
  store ptr %1, ptr %onDismiss.debug.data, align 8, !dbg !345
  store ptr %2, ptr %self.debug, align 8, !dbg !345
  ret i64 0, !dbg !346
}

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGF"(ptr %0, ptr swiftself %1) #0 !dbg !347 {
entry:
  %p.debug = alloca ptr, align 8
    #dbg_declare(ptr %p.debug, !352, !DIExpression(), !354)
  call void @llvm.memset.p0.i64(ptr align 8 %p.debug, i8 0, i64 8, i1 false)
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !353, !DIExpression(), !355)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %access-scratch = alloca [24 x i8], align 8
  %2 = alloca %T7Combine12AnyPublisherVySbs5NeverOG, align 8
  %3 = alloca ptr, align 8
  %access-scratch13 = alloca [24 x i8], align 8
  %4 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgMD") #10, !dbg !356
  %5 = getelementptr inbounds ptr, ptr %4, i64 -1, !dbg !356
  %.valueWitnesses = load ptr, ptr %5, align 8, !dbg !356, !invariant.load !205, !dereferenceable !206
  %6 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses, i32 0, i32 8, !dbg !356
  %size = load i64, ptr %6, align 8, !dbg !356, !invariant.load !205
  %7 = alloca i8, i64 %size, align 16, !dbg !356
  call void @llvm.lifetime.start.p0(i64 -1, ptr %7), !dbg !356
  %8 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGMD") #10, !dbg !356
  %9 = getelementptr inbounds ptr, ptr %8, i64 -1, !dbg !356
  %.valueWitnesses1 = load ptr, ptr %9, align 8, !dbg !356, !invariant.load !205, !dereferenceable !206
  %10 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses1, i32 0, i32 8, !dbg !356
  %size2 = load i64, ptr %10, align 8, !dbg !356, !invariant.load !205
  %11 = alloca i8, i64 %size2, align 16, !dbg !356
  call void @llvm.lifetime.start.p0(i64 -1, ptr %11), !dbg !356
  %12 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGMD") #10, !dbg !356
  %13 = getelementptr inbounds ptr, ptr %12, i64 -1, !dbg !356
  %.valueWitnesses3 = load ptr, ptr %13, align 8, !dbg !356, !invariant.load !205, !dereferenceable !206
  %14 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses3, i32 0, i32 8, !dbg !356
  %size4 = load i64, ptr %14, align 8, !dbg !356, !invariant.load !205
  %15 = alloca i8, i64 %size4, align 16, !dbg !356
  call void @llvm.lifetime.start.p0(i64 -1, ptr %15), !dbg !356
  %16 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGMD") #10, !dbg !356
  %17 = getelementptr inbounds ptr, ptr %16, i64 -1, !dbg !356
  %.valueWitnesses5 = load ptr, ptr %17, align 8, !dbg !356, !invariant.load !205, !dereferenceable !206
  %18 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses5, i32 0, i32 8, !dbg !356
  %size6 = load i64, ptr %18, align 8, !dbg !356, !invariant.load !205
  %19 = alloca i8, i64 %size6, align 16, !dbg !356
  call void @llvm.lifetime.start.p0(i64 -1, ptr %19), !dbg !356
  %20 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGMD") #10, !dbg !356
  %21 = getelementptr inbounds ptr, ptr %20, i64 -1, !dbg !356
  %.valueWitnesses7 = load ptr, ptr %21, align 8, !dbg !356, !invariant.load !205, !dereferenceable !206
  %22 = getelementptr inbounds nuw %swift.vwtable, ptr %.valueWitnesses7, i32 0, i32 8, !dbg !356
  %size8 = load i64, ptr %22, align 8, !dbg !356, !invariant.load !205
  %23 = alloca i8, i64 %size8, align 16, !dbg !356
  call void @llvm.lifetime.start.p0(i64 -1, ptr %23), !dbg !356
  store ptr %0, ptr %p.debug, align 8, !dbg !358
  store ptr %1, ptr %self.debug, align 8, !dbg !358
  %24 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %1, i32 0, i32 2, !dbg !359
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !359
  call void @swift_beginAccess(ptr %24, ptr %access-scratch, i64 32, ptr null) #2, !dbg !359
  %25 = load i64, ptr %24, align 8, !dbg !359
  %26 = icmp eq i64 %25, 0, !dbg !359
  %27 = xor i1 %26, true, !dbg !359
  br i1 %27, label %28, label %66, !dbg !359

28:                                               ; preds = %entry
  %29 = load ptr, ptr %24, align 8, !dbg !359
  %30 = call ptr @swift_retain(ptr returned %29) #3, !dbg !359
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !358
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !358
  call swiftcc void @"$s7Combine14AnyCancellableC6cancelyyF"(ptr swiftself %29), !dbg !360
  call void @swift_release(ptr %29) #2, !dbg !361
  br label %31, !dbg !358

31:                                               ; preds = %28, %66
  %32 = phi i8 [ 1, %66 ], [ 0, %28 ], !dbg !356
  call void @llvm.lifetime.start.p0(i64 8, ptr %2), !dbg !356
  %.box = getelementptr inbounds nuw %T7Combine12AnyPublisherVySbs5NeverOG, ptr %2, i32 0, i32 0, !dbg !363
  store ptr %0, ptr %.box, align 8, !dbg !363
  %33 = call ptr @__swift_instantiateConcreteTypeFromMangledName(ptr @"$s7Combine12AnyPublisherVySbs5NeverOGMD") #10, !dbg !358
  %34 = call ptr @"$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWl"() #12, !dbg !358
  %35 = call swiftcc i64 @"$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiFfA_"(ptr %33, ptr %34), !dbg !358
  call swiftcc void @"$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiF"(ptr noalias sret(%swift.opaque) %11, i64 %35, ptr %33, ptr %34, ptr noalias swiftself %2), !dbg !364
  call void @llvm.lifetime.end.p0(i64 8, ptr %2), !dbg !361
  %36 = call ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWl"() #12, !dbg !365
  call swiftcc void @"$s7Combine9PublisherPAAE6filteryAA10PublishersO6FilterVy_xGSb6OutputQzcF"(ptr noalias sret(%swift.opaque) %15, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFS2bcfU_", ptr null, ptr %8, ptr %36, ptr noalias swiftself %11), !dbg !365
  %37 = getelementptr inbounds ptr, ptr %.valueWitnesses1, i32 1, !dbg !361
  %Destroy = load ptr, ptr %37, align 8, !dbg !361, !invariant.load !205
  call void %Destroy(ptr noalias %11, ptr %8) #2, !dbg !361
  %38 = call ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWl"() #12, !dbg !366
  call swiftcc void @"$s7Combine9PublisherPAAE5firstAA10PublishersO5FirstVy_xGyF"(ptr noalias sret(%swift.opaque) %19, ptr %12, ptr %38, ptr noalias swiftself %15), !dbg !366
  %39 = getelementptr inbounds ptr, ptr %.valueWitnesses3, i32 1, !dbg !361
  %Destroy9 = load ptr, ptr %39, align 8, !dbg !361, !invariant.load !205
  call void %Destroy9(ptr noalias %15, ptr %12) #2, !dbg !361
  %40 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueCMa"(i64 0) #12, !dbg !367
  %41 = extractvalue %swift.metadata_response %40, 0, !dbg !367
  %42 = call swiftcc ptr @"$sSo17OS_dispatch_queueC8DispatchE4mainABvgZ"(ptr swiftself %41), !dbg !368
  call void @llvm.lifetime.start.p0(i64 8, ptr %3), !dbg !356
  store ptr %42, ptr %3, align 8, !dbg !368
  %43 = call swiftcc %swift.metadata_response @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVMa"(i64 0) #12, !dbg !358
  %44 = extractvalue %swift.metadata_response %43, 0, !dbg !358
  %45 = getelementptr inbounds ptr, ptr %44, i64 -1, !dbg !358
  %.valueWitnesses10 = load ptr, ptr %45, align 8, !dbg !358, !invariant.load !205, !dereferenceable !206
  %46 = getelementptr inbounds ptr, ptr %.valueWitnesses10, i32 7, !dbg !358
  %StoreEnumTagSinglePayload = load ptr, ptr %46, align 8, !dbg !358, !invariant.load !205
  call void %StoreEnumTagSinglePayload(ptr noalias %7, i32 1, i32 1, ptr %44) #2, !dbg !358
  %47 = call ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWl"() #12, !dbg !369
  %48 = call ptr @"$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWl"() #12, !dbg !369
  call swiftcc void @"$s7Combine9PublisherPAAE7receive2on7optionsAA10PublishersO9ReceiveOnVy_xqd__Gqd___16SchedulerOptionsQyd__SgtAA0I0Rd__lF"(ptr noalias sret(%swift.opaque) %23, ptr noalias %3, ptr noalias %7, ptr %16, ptr %41, ptr %47, ptr %48, ptr noalias swiftself %19), !dbg !369
  %49 = call ptr @"$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgWOh"(ptr %7), !dbg !361
  %toDestroy = load ptr, ptr %3, align 8, !dbg !361
  call void @llvm.objc.release(ptr %toDestroy), !dbg !361
  call void @llvm.lifetime.end.p0(i64 8, ptr %3), !dbg !361
  %50 = getelementptr inbounds ptr, ptr %.valueWitnesses5, i32 1, !dbg !361
  %Destroy11 = load ptr, ptr %50, align 8, !dbg !361, !invariant.load !205
  call void %Destroy11(ptr noalias %19, ptr %16) #2, !dbg !361
  %51 = call noalias ptr @swift_allocObject(ptr getelementptr inbounds (%swift.full_boxmetadata, ptr @metadata, i32 0, i32 2), i64 24, i64 7) #2, !dbg !370
  %52 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %51, i32 0, i32 1, !dbg !370
  %53 = call ptr @swift_retain(ptr returned %1) #3, !dbg !373
  %54 = ptrtoint ptr %1 to i64, !dbg !358
  %55 = inttoptr i64 %54 to ptr, !dbg !358
  %56 = call ptr @swift_weakInit(ptr returned %52, ptr %55) #2, !dbg !358
  %57 = inttoptr i64 %54 to ptr, !dbg !358
  call void @swift_release(ptr %57) #2, !dbg !358
  %58 = call ptr @swift_retain(ptr returned %51) #3, !dbg !358
  call void @swift_release(ptr %51) #2, !dbg !361
  %59 = call ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWl"() #12, !dbg !374
  %60 = call swiftcc ptr @"$s7Combine9PublisherPAAs5NeverO7FailureRtzrlE4sink12receiveValueAA14AnyCancellableCy6OutputQzc_tF"(ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_TA", ptr %51, ptr %20, ptr %59, ptr noalias swiftself %23), !dbg !374
  call void @swift_release(ptr %51) #2, !dbg !361
  %61 = getelementptr inbounds ptr, ptr %.valueWitnesses7, i32 1, !dbg !361
  %Destroy12 = load ptr, ptr %61, align 8, !dbg !361, !invariant.load !205
  call void %Destroy12(ptr noalias %23, ptr %20) #2, !dbg !361
  %62 = ptrtoint ptr %60 to i64, !dbg !358
  %63 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %1, i32 0, i32 2, !dbg !375
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch13), !dbg !375
  call void @swift_beginAccess(ptr %63, ptr %access-scratch13, i64 33, ptr null) #2, !dbg !375
  %64 = load i64, ptr %63, align 8, !dbg !375
  store i64 %62, ptr %63, align 8, !dbg !375
  %65 = inttoptr i64 %64 to ptr, !dbg !375
  call void @swift_release(ptr %65) #2, !dbg !375
  call void @swift_endAccess(ptr %access-scratch13) #2, !dbg !358
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch13), !dbg !358
  call void @llvm.lifetime.end.p0(i64 -1, ptr %23), !dbg !376
  call void @llvm.lifetime.end.p0(i64 -1, ptr %19), !dbg !376
  call void @llvm.lifetime.end.p0(i64 -1, ptr %15), !dbg !376
  call void @llvm.lifetime.end.p0(i64 -1, ptr %11), !dbg !376
  call void @llvm.lifetime.end.p0(i64 -1, ptr %7), !dbg !376
  ret void, !dbg !376

66:                                               ; preds = %entry
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !358
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !358
  br label %31, !dbg !358
}

define linkonce_odr hidden swiftcc i64 @"$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiFfA_"(ptr %Self, ptr %Self.Publisher) #0 !dbg !377 {
entry:
  %Self1 = alloca ptr, align 8
    #dbg_declare(ptr %Self1, !383, !DIExpression(), !387)
  store ptr %Self, ptr %Self1, align 8
  ret i64 1, !dbg !387
}

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWl"() #8 !dbg !388 {
entry:
  %0 = load ptr, ptr @"$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWL", align 8, !dbg !389
  %1 = icmp eq ptr %0, null, !dbg !389
  br i1 %1, label %cacheIsNull, label %cont, !dbg !389

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine12AnyPublisherVySbs5NeverOGMD") #10, !dbg !389
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine12AnyPublisherVyxq_GAA0C0AAMc", ptr %2, ptr undef) #10, !dbg !389
  store atomic ptr %3, ptr @"$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWL" release, align 8, !dbg !389
  br label %cont, !dbg !389

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !389
  ret ptr %4, !dbg !389
}

declare swiftcc void @"$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiF"(ptr noalias sret(%swift.opaque), i64, ptr, ptr, ptr noalias swiftself) #0

define internal swiftcc i1 @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFS2bcfU_"(ptr noalias captures(none) dereferenceable(1) %0) #0 !dbg !390 {
entry:
  %"$0.debug" = alloca i1, align 8
    #dbg_declare(ptr %"$0.debug", !394, !DIExpression(), !396)
  call void @llvm.memset.p0.i64(ptr align 8 %"$0.debug", i8 0, i64 1, i1 false)
  %._value = getelementptr inbounds nuw %TSb, ptr %0, i32 0, i32 0, !dbg !397
  %1 = load i1, ptr %._value, align 1, !dbg !397
  store i1 %1, ptr %"$0.debug", align 8, !dbg !399
  %2 = icmp eq i1 %1, true, !dbg !400
  ret i1 %2, !dbg !401
}

declare swiftcc void @"$s7Combine9PublisherPAAE6filteryAA10PublishersO6FilterVy_xGSb6OutputQzcF"(ptr noalias sret(%swift.opaque), ptr, ptr, ptr, ptr, ptr noalias swiftself) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWl"() #8 !dbg !402 {
entry:
  %0 = load ptr, ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWL", align 8, !dbg !403
  %1 = icmp eq ptr %0, null, !dbg !403
  br i1 %1, label %cacheIsNull, label %cont, !dbg !403

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGMD") #10, !dbg !403
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine10PublishersO4DropVy_xGAA9PublisherAAMc", ptr %2, ptr undef) #10, !dbg !403
  store atomic ptr %3, ptr @"$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWL" release, align 8, !dbg !403
  br label %cont, !dbg !403

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !403
  ret ptr %4, !dbg !403
}

declare swiftcc void @"$s7Combine9PublisherPAAE5firstAA10PublishersO5FirstVy_xGyF"(ptr noalias sret(%swift.opaque), ptr, ptr, ptr noalias swiftself) #0

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWl"() #8 !dbg !404 {
entry:
  %0 = load ptr, ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWL", align 8, !dbg !405
  %1 = icmp eq ptr %0, null, !dbg !405
  br i1 %1, label %cacheIsNull, label %cont, !dbg !405

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGMD") #10, !dbg !405
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine10PublishersO6FilterVy_xGAA9PublisherAAMc", ptr %2, ptr undef) #10, !dbg !405
  store atomic ptr %3, ptr @"$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWL" release, align 8, !dbg !405
  br label %cont, !dbg !405

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !405
  ret ptr %4, !dbg !405
}

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWl"() #8 !dbg !406 {
entry:
  %0 = load ptr, ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWL", align 8, !dbg !407
  %1 = icmp eq ptr %0, null, !dbg !407
  br i1 %1, label %cacheIsNull, label %cont, !dbg !407

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGMD") #10, !dbg !407
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine10PublishersO5FirstVy_xGAA9PublisherAAMc", ptr %2, ptr undef) #10, !dbg !407
  store atomic ptr %3, ptr @"$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWL" release, align 8, !dbg !407
  br label %cont, !dbg !407

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !407
  ret ptr %4, !dbg !407
}

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_"(ptr noalias captures(none) dereferenceable(1) %0, ptr %1) #0 !dbg !408 {
entry:
  %_0.debug = alloca i1, align 8
    #dbg_declare(ptr %_0.debug, !412, !DIExpression(), !418)
  call void @llvm.memset.p0.i64(ptr align 8 %_0.debug, i8 0, i64 1, i1 false)
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !414, !DIExpression(DW_OP_deref), !419)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  %access-scratch = alloca [24 x i8], align 8
  %self.debug1 = alloca ptr, align 8
    #dbg_declare(ptr %self.debug1, !415, !DIExpression(), !420)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug1, i8 0, i64 8, i1 false)
  %access-scratch2 = alloca [24 x i8], align 8
  %._value = getelementptr inbounds nuw %TSb, ptr %0, i32 0, i32 0, !dbg !421
  %2 = load i1, ptr %._value, align 1, !dbg !421
  store i1 %2, ptr %_0.debug, align 8, !dbg !423
  %3 = getelementptr inbounds nuw <{ %swift.refcounted, %swift.weak }>, ptr %1, i32 0, i32 1, !dbg !419
  store ptr %3, ptr %self.debug, align 8, !dbg !424
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch), !dbg !425
  call void @swift_beginAccess(ptr %3, ptr %access-scratch, i64 32, ptr null) #2, !dbg !425
  %4 = call ptr @swift_weakLoadStrong(ptr %3) #2, !dbg !425
  %5 = ptrtoint ptr %4 to i64, !dbg !425
  call void @swift_endAccess(ptr %access-scratch) #2, !dbg !424
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch), !dbg !424
  %6 = icmp eq i64 %5, 0, !dbg !425
  br i1 %6, label %9, label %7, !dbg !425

7:                                                ; preds = %entry
  %8 = inttoptr i64 %5 to ptr, !dbg !425
  br label %10, !dbg !425

9:                                                ; preds = %entry
  br label %16, !dbg !425

10:                                               ; preds = %7
  %11 = phi ptr [ %8, %7 ], !dbg !426
  store ptr %11, ptr %self.debug1, align 8, !dbg !428
  %12 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %11, i32 0, i32 4, !dbg !429
  call void @llvm.lifetime.start.p0(i64 -1, ptr %access-scratch2), !dbg !429
  call void @swift_beginAccess(ptr %12, ptr %access-scratch2, i64 32, ptr null) #2, !dbg !429
  %._value3 = getelementptr inbounds nuw %TSb, ptr %12, i32 0, i32 0, !dbg !429
  %13 = load i1, ptr %._value3, align 8, !dbg !429
  call void @swift_endAccess(ptr %access-scratch2) #2, !dbg !428
  call void @llvm.lifetime.end.p0(i64 -1, ptr %access-scratch2), !dbg !428
  br i1 %13, label %14, label %15, !dbg !424

14:                                               ; preds = %10
  call void asm sideeffect "nop", ""(), !dbg !424
  call swiftcc void @"$s10RcExtraLog0A4Info_4file8function4lineySSyXK_s12StaticStringVAGs5Int32VtF"(ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_SSyXEfu_", ptr null, i64 ptrtoint (ptr @".str.48.RcChainedSinkReproSwiftBitcodeFix/tth_min5.swift" to i64), i64 48, i8 2, i64 ptrtoint (ptr @".str.15.resubscribe(_:)" to i64), i64 15, i8 2, i32 31), !dbg !430
  call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF"(ptr swiftself %11), !dbg !431
  call void @swift_release(ptr %11) #2, !dbg !432
  br label %17, !dbg !432

15:                                               ; preds = %10
  call void @swift_release(ptr %11) #2, !dbg !425
  br label %16, !dbg !425

16:                                               ; preds = %15, %9
  br label %17, !dbg !433

17:                                               ; preds = %14, %16
  ret void, !dbg !432
}

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_TA"(ptr noalias captures(none) dereferenceable(1) %0, ptr swiftself %1) #0 !dbg !435 {
entry:
  tail call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_"(ptr noalias captures(none) dereferenceable(1) %0, ptr %1), !dbg !436
  ret void, !dbg !436
}

; Function Attrs: noinline nounwind memory(none)
define linkonce_odr hidden ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWl"() #8 !dbg !437 {
entry:
  %0 = load ptr, ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWL", align 8, !dbg !438
  %1 = icmp eq ptr %0, null, !dbg !438
  br i1 %1, label %cacheIsNull, label %cont, !dbg !438

cacheIsNull:                                      ; preds = %entry
  %2 = call ptr @__swift_instantiateConcreteTypeFromMangledNameAbstract(ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGMD") #10, !dbg !438
  %3 = call ptr @swift_getWitnessTable(ptr @"$s7Combine10PublishersO9ReceiveOnVy_xq_GAA9PublisherAAMc", ptr %2, ptr undef) #10, !dbg !438
  store atomic ptr %3, ptr @"$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWL" release, align 8, !dbg !438
  br label %cont, !dbg !438

cont:                                             ; preds = %cacheIsNull, %entry
  %4 = phi ptr [ %0, %entry ], [ %3, %cacheIsNull ], !dbg !438
  ret ptr %4, !dbg !438
}

declare swiftcc void @"$s7Combine14AnyCancellableC6cancelyyF"(ptr swiftself) #0

define internal swiftcc { i64, ptr } @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_SSyXEfu_"() #0 !dbg !439 {
entry:
  %0 = call swiftcc { i64, ptr } @"$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC"(ptr @.str.1.x, i64 1, i1 true), !dbg !443
  %1 = extractvalue { i64, ptr } %0, 0, !dbg !443
  %2 = extractvalue { i64, ptr } %0, 1, !dbg !443
  %3 = insertvalue { i64, ptr } undef, i64 %1, 0, !dbg !443
  %4 = insertvalue { i64, ptr } %3, ptr %2, 1, !dbg !443
  ret { i64, ptr } %4, !dbg !443
}

define linkonce_odr hidden swiftcc void @"$s10RcExtraLog0A4Info_4file8function4lineySSyXK_s12StaticStringVAGs5Int32VtF"(ptr %0, ptr %1, i64 %2, i64 %3, i8 %4, i64 %5, i64 %6, i8 %7, i32 %8) #0 !dbg !444 {
entry:
  call swiftcc void @"$s10RcExtraLog10trampolineyySSyXE_AA8LogLevelOs12StaticStringVAFs5Int32VtF"(ptr %0, ptr %1, i8 2, i64 %2, i64 %3, i8 %4, i64 %5, i64 %6, i8 %7, i32 %8), !dbg !450
  ret void, !dbg !450
}

define hidden swiftcc ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfd"(ptr swiftself %0) #0 !dbg !451 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !457, !DIExpression(), !458)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  store ptr %0, ptr %self.debug, align 8, !dbg !459
  %1 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %0, i32 0, i32 1, !dbg !459
  %2 = call ptr @"$s7Combine14AnyCancellableCSgWOh"(ptr %1), !dbg !459
  %3 = getelementptr inbounds nuw %T33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC, ptr %0, i32 0, i32 2, !dbg !459
  %4 = call ptr @"$s7Combine14AnyCancellableCSgWOh"(ptr %3), !dbg !459
  ret ptr %0, !dbg !459
}

define hidden swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfD"(ptr swiftself %0) #0 !dbg !460 {
entry:
  %self.debug = alloca ptr, align 8
    #dbg_declare(ptr %self.debug, !463, !DIExpression(), !464)
  call void @llvm.memset.p0.i64(ptr align 8 %self.debug, i8 0, i64 8, i1 false)
  store ptr %0, ptr %self.debug, align 8, !dbg !465
  %1 = call swiftcc ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfd"(ptr swiftself %0), !dbg !465
  call void @swift_deallocClassInstance(ptr %1, i64 41, i64 7) #2, !dbg !465
  ret void, !dbg !465
}

; Function Attrs: noinline nounwind memory(none)
define hidden swiftcc %swift.metadata_response @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMa"(i64 %0) #8 !dbg !466 {
entry:
  %1 = call ptr @objc_opt_self(ptr getelementptr inbounds (<{ ptr, ptr, ptr, i64, ptr, ptr, ptr, i64, i32, i32, i32, i16, i16, i32, i32, ptr, ptr, i64, i64, i64, i64, ptr }>, ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMf", i32 0, i32 3)) #2, !dbg !467
  %2 = insertvalue %swift.metadata_response undef, ptr %1, 0, !dbg !467
  %3 = insertvalue %swift.metadata_response %2, i64 0, 1, !dbg !467
  ret %swift.metadata_response %3, !dbg !467
}





















; Function Attrs: nounwind
declare void @swift_deallocClassInstance(ptr, i64, i64) #2

; Function Attrs: noinline nounwind
define linkonce_odr hidden ptr @"$s7Combine14AnyCancellableCSgWOh"(ptr %0) #11 !dbg !468 {
entry:
  %1 = load ptr, ptr %0, align 8, !dbg !469
  call void @swift_release(ptr %1) #2, !dbg !469
  ret ptr %0, !dbg !469
}

; Function Attrs: nounwind willreturn
declare ptr @swift_weakLoadStrong(ptr) #3

declare swiftcc void @"$s10RcExtraLog10trampolineyySSyXE_AA8LogLevelOs12StaticStringVAFs5Int32VtF"(ptr, ptr, i8, i64, i64, i8, i64, i64, i8, i32) #0

declare swiftcc { i64, ptr } @"$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC"(ptr, i64, i1) #0

define internal swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_TA"(ptr swiftself %0) #0 !dbg !470 {
entry:
  tail call swiftcc void @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_"(ptr %0), !dbg !471
  ret void, !dbg !471
}

; Function Attrs: noinline nounwind
define linkonce_odr hidden ptr @"$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgWOh"(ptr %0) #11 !dbg !472 {
entry:
  %1 = load ptr, ptr %0, align 8, !dbg !473
  call void @swift_release(ptr %1) #2, !dbg !473
  ret ptr %0, !dbg !473
}

attributes #0 = { "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { nounwind }
attributes #3 = { nounwind willreturn }
attributes #4 = { noinline nounwind willreturn memory(read) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #5 = { nocallback nofree nosync nounwind willreturn memory(none) }
attributes #6 = { nounwind memory(argmem: readwrite) }
attributes #7 = { nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #8 = { noinline nounwind memory(none) "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #9 = { nounwind willreturn memory(none) }
attributes #10 = { nounwind memory(read) }
attributes #11 = { noinline nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+altnzcv,+ccdp,+ccidx,+ccpp,+complxnum,+crc,+dit,+dotprod,+flagm,+fp-armv8,+fp16fml,+fptoint,+fullfp16,+jsconv,+lse,+neon,+pauth,+perfmon,+predres,+ras,+rcpc,+rdm,+sb,+sha2,+sha3,+specrestrict,+ssbs,+v8.1a,+v8.2a,+v8.3a,+v8.4a,+v8a" }
attributes #12 = { nounwind memory(none) }
attributes #13 = { nounwind memory(argmem: read) }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6, !7, !8, !9, !10, !11, !12, !13, !14}
!llvm.dbg.cu = !{!15, !51, !53, !55}
!swift.module.flags = !{!57}
!llvm.linker.options = !{!58, !59, !60, !61, !62, !63, !64, !65, !66, !67, !68, !69, !70, !71, !72, !73, !74, !75, !76, !77, !78, !79, !80, !81, !82, !83, !84, !85, !86, !87, !88, !89, !90, !91, !92, !93, !94, !95, !96, !97, !98, !99, !100, !101, !102, !103, !104, !105, !106, !107, !108, !109, !110, !111, !112, !113, !114, !115, !116, !117, !118, !119, !120, !121, !122, !123, !124, !125, !126, !127, !128, !129, !130, !131}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 26, i32 2]}
!1 = !{i32 1, !"Objective-C Version", i32 2}
!2 = !{i32 1, !"Objective-C Image Info Version", i32 0}
!3 = !{i32 1, !"Objective-C Image Info Section", !"__DATA,__objc_imageinfo,regular,no_dead_strip"}
!4 = !{i32 4, !"Objective-C Garbage Collection", i32 100861696}
!5 = !{i32 1, !"Objective-C Is Simulated", i32 32}
!6 = !{i32 1, !"Objective-C Class Properties", i32 64}
!7 = !{i32 1, !"Objective-C Enforce ClassRO Pointer Signing", i8 0}
!8 = !{i32 7, !"Dwarf Version", i32 4}
!9 = !{i32 2, !"Debug Info Version", i32 3}
!10 = !{i32 1, !"wchar_size", i32 4}
!11 = !{i32 8, !"PIC Level", i32 2}
!12 = !{i32 7, !"uwtable", i32 1}
!13 = !{i32 7, !"frame-pointer", i32 1}
!14 = !{i32 1, !"Swift Version", i32 7}
!15 = distinct !DICompileUnit(language: DW_LANG_Swift, file: !16, producer: "Swift version (redacted)", isOptimized: false, runtimeVersion: 6, emissionKind: FullDebug, retainedTypes: !17, imports: !32, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!16 = !DIFile(filename: "infer/tests/codetoanalyze/swift/bitcode-handcrafted-pulse/rc_chained_sink_weak_self.swift", directory: ".")
!17 = !{!18, !26, !28, !30}
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s7Combine10PublishersO4DropV6Outputay_AA12AnyPublisherVySbs5NeverOG_GD", scope: !20, file: !19, baseType: !23)
!19 = !DIFile(filename: "build/cache/", directory: ".")
!20 = !DICompositeType(tag: DW_TAG_structure_type, name: "Drop", scope: !21, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine10PublishersO4DropVy_xGD")
!21 = !DICompositeType(tag: DW_TAG_structure_type, name: "Publishers", scope: !22, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine10PublishersOD")
!22 = !DIModule(scope: null, name: "Combine", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!23 = !DICompositeType(tag: DW_TAG_structure_type, name: "Bool", scope: !25, file: !24, size: 8, runtimeLang: DW_LANG_Swift, identifier: "$sSbD")
!24 = !DIFile(filename: "build/cache/", directory: ".")
!25 = !DIModule(scope: null, name: "Swift", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!26 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s7Combine10PublishersO6FilterV6Outputay_AC4DropVy_AA12AnyPublisherVySbs5NeverOGG_GD", scope: !27, file: !19, baseType: !18)
!27 = !DICompositeType(tag: DW_TAG_structure_type, name: "Filter", scope: !21, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine10PublishersO6FilterVy_xGD")
!28 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s7Combine10PublishersO5FirstV6Outputay_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGG_GD", scope: !29, file: !19, baseType: !26)
!29 = !DICompositeType(tag: DW_TAG_structure_type, name: "First", scope: !21, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine10PublishersO5FirstVy_xGD")
!30 = !DIDerivedType(tag: DW_TAG_typedef, name: "$ss4VoidaD", scope: !25, file: !24, baseType: !31)
!31 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sytD", file: !16, runtimeLang: DW_LANG_Swift, identifier: "$sytD")
!32 = !{!33, !35, !36, !37, !39, !41, !43, !45, !47, !49}
!33 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !34, file: !16)
!34 = !DIModule(scope: null, name: "RcChainedSinkReproSwiftBitcodeFix", includePath: "/tmp")
!35 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !25, file: !16)
!36 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !22, file: !16, line: 1)
!37 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !38, file: !16, line: 2)
!38 = !DIModule(scope: null, name: "FakeFrameworkForChainedSinkBitcodeTest0", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!39 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !40, file: !16, line: 3)
!40 = !DIModule(scope: null, name: "RcExtraLog", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!41 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !42, file: !16)
!42 = !DIModule(scope: null, name: "_StringProcessing", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!43 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !44, file: !16)
!44 = !DIModule(scope: null, name: "_SwiftConcurrencyShims", configMacros: "\22-DDEBUG=1\22", includePath: ".")
!45 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !46, file: !16)
!46 = !DIModule(scope: null, name: "_Concurrency", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!47 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !48, file: !16)
!48 = !DIModule(scope: null, name: "SwiftOnoneSupport", configMacros: "\22-DDEBUG=1\22", includePath: "build/cache/")
!49 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !16, entity: !50, file: !16)
!50 = !DIModule(scope: null, name: "TestFramework0Export", configMacros: "\22-DDEBUG=1\22", includePath: ".")
!51 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !52, producer: "clang version (redacted)", isOptimized: false, runtimeVersion: 2, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: Apple, sysroot: "build/cache/", sdk: "iPhoneSimulator26.2.sdk")
!52 = !DIFile(filename: "<swift-imported-modules>", directory: ".")
!53 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !54, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!54 = !DIFile(filename: "_SwiftConcurrencyShims", directory: ".")
!55 = distinct !DICompileUnit(language: DW_LANG_ObjC, file: !56, producer: "Swift version (redacted)", isOptimized: true, runtimeVersion: 0, splitDebugFilename: "build/cache/", emissionKind: FullDebug, dwoId: 18446744073709551614)
!56 = !DIFile(filename: "TestFramework0Export", directory: ".")
!57 = !{!"standard-library", i1 false}
!58 = !{!"-lswiftUIKit"}
!59 = !{!"-framework", !"UIKit"}
!60 = !{!"-lswiftCore"}
!61 = !{!"-lswift_StringProcessing"}
!62 = !{!"-lswift_Concurrency"}
!63 = !{!"-framework", !"UIUtilities"}
!64 = !{!"-lswiftFoundation"}
!65 = !{!"-framework", !"Foundation"}
!66 = !{!"-lswift_DarwinFoundation3"}
!67 = !{!"-lswift_DarwinFoundation1"}
!68 = !{!"-lswift_DarwinFoundation2"}
!69 = !{!"-lswiftSystem"}
!70 = !{!"-lswiftDarwin"}
!71 = !{!"-lswift_Builtin_float"}
!72 = !{!"-lswiftObservation"}
!73 = !{!"-lswiftObjectiveC"}
!74 = !{!"-lswiftCoreFoundation"}
!75 = !{!"-framework", !"CoreFoundation"}
!76 = !{!"-lswiftDispatch"}
!77 = !{!"-framework", !"Combine"}
!78 = !{!"-framework", !"Security"}
!79 = !{!"-lswiftXPC"}
!80 = !{!"-framework", !"CFNetwork"}
!81 = !{!"-framework", !"CoreGraphics"}
!82 = !{!"-lswiftQuartzCore"}
!83 = !{!"-framework", !"QuartzCore"}
!84 = !{!"-framework", !"CoreVideo"}
!85 = !{!"-framework", !"IOSurface"}
!86 = !{!"-lswiftMetal"}
!87 = !{!"-framework", !"Metal"}
!88 = !{!"-framework", !"OpenGLES"}
!89 = !{!"-lswiftCoreImage"}
!90 = !{!"-framework", !"CoreImage"}
!91 = !{!"-framework", !"ImageIO"}
!92 = !{!"-framework", !"CoreText"}
!93 = !{!"-lswiftUniformTypeIdentifiers"}
!94 = !{!"-framework", !"UniformTypeIdentifiers"}
!95 = !{!"-framework", !"Symbols"}
!96 = !{!"-framework", !"UserNotifications"}
!97 = !{!"-framework", !"FileProvider"}
!98 = !{!"-framework", !"SwiftUICore"}
!99 = !{!"-lswiftos"}
!100 = !{!"-lswiftOSLog"}
!101 = !{!"-framework", !"OSLog"}
!102 = !{!"-framework", !"DeveloperToolsSupport"}
!103 = !{!"-framework", !"CoreTransferable"}
!104 = !{!"-framework", !"Accessibility"}
!105 = !{!"-framework", !"DataDetection"}
!106 = !{!"-lswiftSwiftOnoneSupport"}
!107 = !{!"-framework", !"SwiftUI"}
!108 = !{!"-lswiftSpatial"}
!109 = !{!"-lswiftsimd"}
!110 = !{!"-framework", !"CoreData"}
!111 = !{!"-lswiftAVFoundation"}
!112 = !{!"-lswiftSynchronization"}
!113 = !{!"-lswiftCoreMedia"}
!114 = !{!"-framework", !"CoreMedia"}
!115 = !{!"-lswiftCoreAudio"}
!116 = !{!"-framework", !"CoreAudio"}
!117 = !{!"-framework", !"AVFoundation"}
!118 = !{!"-framework", !"AVFAudio"}
!119 = !{!"-framework", !"AudioToolbox"}
!120 = !{!"-lswiftCoreMIDI"}
!121 = !{!"-framework", !"CoreMIDI"}
!122 = !{!"-framework", !"AVRouting"}
!123 = !{!"-lswiftNetwork"}
!124 = !{!"-framework", !"Network"}
!125 = !{!"-lswiftDistributed"}
!126 = !{!"-framework", !"MediaToolbox"}
!127 = !{!"-framework", !"CoreBluetooth"}
!128 = !{!"-lz"}
!129 = !{!"-lswiftCompression"}
!130 = !{!"-lcompression"}
!131 = !{!"-lobjc"}
!133 = !DIFile(filename: "<compiler-generated>", directory: "")
!134 = !DICompositeType(tag: DW_TAG_structure_type, name: "ChainedSinkRepro", scope: !34, file: !16, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCD")
!154 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int", scope: !25, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSiD")
!172 = distinct !DISubprogram(name: "init", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfC", scope: !134, file: !16, line: 16, type: !173, scopeLine: 16, spFlags: DISPFlagDefinition, unit: !15, declaration: !179)
!173 = !DISubroutineType(types: !174)
!174 = !{!134, !175, !177, !178}
!175 = !DICompositeType(tag: DW_TAG_structure_type, name: "SearchAndSummarizationToggleTimeout", scope: !38, file: !176, size: 64, runtimeLang: DW_LANG_Swift, identifier: "$s39FakeFrameworkForChainedSinkBitcodeTest00bcD13ToggleTimeoutCD")
!176 = !DIFile(filename: "build/cache/", directory: ".")
!177 = !DICompositeType(tag: DW_TAG_structure_type, name: "AnyPublisher", scope: !22, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine12AnyPublisherVySbs5NeverOGD")
!178 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCXMTD", size: 64, flags: DIFlagArtificial, runtimeLang: DW_LANG_Swift, identifier: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCXMTD")
!179 = !DISubprogram(name: "init", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfC", scope: !134, file: !16, line: 16, type: !173, scopeLine: 16, spFlags: 0)
!180 = !DILocation(line: 0, scope: !172)
!181 = distinct !DISubprogram(name: "init", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfc", scope: !134, file: !16, line: 16, type: !182, scopeLine: 16, spFlags: DISPFlagDefinition, unit: !15, declaration: !184, retainedNodes: !185)
!182 = !DISubroutineType(types: !183)
!183 = !{!134, !175, !177, !134}
!184 = !DISubprogram(name: "init", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfc", scope: !134, file: !16, line: 16, type: !182, scopeLine: 16, spFlags: 0)
!185 = !{!186, !188, !198}
!186 = !DILocalVariable(name: "timeout", arg: 1, scope: !181, file: !16, line: 16, type: !187)
!187 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !175)
!188 = !DILocalVariable(name: "p", arg: 2, scope: !181, file: !16, line: 16, type: !189)
!189 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !190)
!190 = !DICompositeType(tag: DW_TAG_structure_type, scope: !22, file: !19, size: 64, elements: !191, runtimeLang: DW_LANG_Swift)
!191 = !{!192}
!192 = !DIDerivedType(tag: DW_TAG_member, scope: !22, file: !19, baseType: !193, size: 64)
!193 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s7Combine12AnyPublisherVySbs5NeverOGD", scope: !22, file: !19, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: !194)
!194 = !{!195, !196}
!195 = !DITemplateTypeParameter(type: !23)
!196 = !DITemplateTypeParameter(type: !197)
!197 = !DICompositeType(tag: DW_TAG_structure_type, name: "Never", scope: !25, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$ss5NeverOD")
!198 = !DILocalVariable(name: "self", arg: 3, scope: !181, file: !16, line: 16, type: !199, flags: DIFlagArtificial)
!199 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !134)
!200 = !DILocation(line: 16, column: 8, scope: !181)
!201 = !DILocation(line: 16, column: 54, scope: !181)
!202 = !DILocation(line: 16, column: 3, scope: !181)
!203 = !DILocation(line: 0, scope: !204)
!204 = !DILexicalBlockFile(scope: !181, discriminator: 0)
!205 = !{}
!206 = !{i64 96}
!207 = !DILocation(line: 0, scope: !181)
!208 = !DILocation(line: 11, column: 15, scope: !209)
!209 = distinct !DILexicalBlock(scope: !210, file: !16, line: 11, column: 15)
!210 = distinct !DILexicalBlock(scope: !211, file: !16, line: 10, column: 30)
!211 = distinct !DILexicalBlock(scope: !181, file: !16, line: 10, column: 1)
!212 = !DILocation(line: 0, scope: !213)
!213 = !DILexicalBlockFile(scope: !209, file: !133, discriminator: 0)
!214 = !DILocation(line: 12, column: 15, scope: !215)
!215 = distinct !DILexicalBlock(scope: !210, file: !16, line: 12, column: 15)
!216 = !DILocation(line: 0, scope: !217)
!217 = !DILexicalBlockFile(scope: !215, file: !133, discriminator: 0)
!218 = !DILocation(line: 13, column: 15, scope: !219)
!219 = distinct !DILexicalBlock(scope: !210, file: !16, line: 13, column: 15)
!220 = !DILocation(line: 13, column: 36, scope: !221)
!221 = distinct !DILexicalBlock(scope: !210, file: !16, line: 13, column: 36)
!222 = !DILocation(line: 14, column: 15, scope: !223)
!223 = distinct !DILexicalBlock(scope: !210, file: !16, line: 14, column: 15)
!224 = !DILocation(line: 14, column: 28, scope: !225)
!225 = distinct !DILexicalBlock(scope: !210, file: !16, line: 14, column: 28)
!226 = !DILocation(line: 17, column: 18, scope: !181)
!227 = !DILocation(line: 17, column: 38, scope: !181)
!228 = !DILocation(line: 17, column: 52, scope: !181)
!229 = !DILocation(line: 17, column: 26, scope: !181)
!230 = !DILocation(line: 0, scope: !231)
!231 = !DILexicalBlockFile(scope: !181, file: !133, discriminator: 0)
!232 = !DILocation(line: 0, scope: !233)
!233 = !DILexicalBlockFile(scope: !234, discriminator: 0)
!234 = distinct !DILexicalBlock(scope: !181, file: !16, line: 18, column: 7)
!235 = !DILocation(line: 17, column: 71, scope: !181)
!236 = !DILocation(line: 17, column: 63, scope: !181)
!237 = !DILocation(line: 17, column: 58, scope: !181)
!238 = !DILocation(line: 17, column: 8, scope: !181)
!239 = !DILocation(line: 23, column: 5, scope: !181)
!240 = !DILocation(line: 24, column: 3, scope: !181)
!241 = distinct !DISubprogram(linkageName: "__swift_instantiateConcreteTypeFromMangledName", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!242 = !DISubroutineType(types: null)
!243 = !DILocation(line: 0, scope: !241)
!244 = distinct !DISubprogram(linkageName: "$sSo17OS_dispatch_queueCMa", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!245 = !DILocation(line: 0, scope: !244)
!246 = distinct !DISubprogram(linkageName: "$s7Combine12AnyPublisherVyyts5NeverOGACyxq_GAA0C0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!247 = !DILocation(line: 0, scope: !246)
!248 = distinct !DISubprogram(linkageName: "__swift_instantiateConcreteTypeFromMangledNameAbstract", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!249 = !DILocation(line: 0, scope: !248)
!250 = distinct !DISubprogram(linkageName: "$sSo17OS_dispatch_queueCAB7Combine9Scheduler8DispatchWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!251 = !DILocation(line: 0, scope: !250)
!252 = distinct !DISubprogram(linkageName: "$sSo17OS_dispatch_queueC8DispatchE16SchedulerOptionsVSgWOh", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!253 = !DILocation(line: 0, scope: !252)
!254 = distinct !DISubprogram(linkageName: "$s7Combine12AnyPublisherVyyts5NeverOGWOh", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!255 = !DILocation(line: 0, scope: !254)
!256 = distinct !DISubprogram(linkageName: "objectdestroy", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!257 = !DILocation(line: 0, scope: !256)
!258 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_", scope: !134, file: !16, line: 17, type: !259, scopeLine: 17, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, declaration: !263, retainedNodes: !264)
!259 = !DISubroutineType(types: !260)
!260 = !{!31, !31, !261, !262}
!261 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgXwz_XxD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgXwz_XxD")
!262 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCXDXMTD", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCXDXMTD")
!263 = !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_", scope: !134, file: !16, line: 17, type: !259, scopeLine: 17, spFlags: DISPFlagLocalToUnit)
!264 = !{!265, !267, !275}
!265 = !DILocalVariable(name: "_0", arg: 1, scope: !258, file: !16, line: 17, type: !266)
!266 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !30)
!267 = !DILocalVariable(name: "self", arg: 2, scope: !258, file: !16, line: 17, type: !268, flags: DIFlagArtificial)
!268 = !DIDerivedType(tag: DW_TAG_typedef, name: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCXSqXwD", scope: !16, baseType: !269)
!269 = !DICompositeType(tag: DW_TAG_structure_type, scope: !25, file: !24, size: 64, elements: !270, runtimeLang: DW_LANG_Swift)
!270 = !{!271}
!271 = !DIDerivedType(tag: DW_TAG_member, scope: !25, file: !24, baseType: !272, size: 64)
!272 = !DICompositeType(tag: DW_TAG_structure_type, name: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgD", scope: !25, file: !24, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, templateParams: !273)
!273 = !{!274}
!274 = !DITemplateTypeParameter(type: !134)
!275 = !DILocalVariable(name: "self", scope: !276, file: !16, line: 18, type: !199)
!276 = distinct !DILexicalBlock(scope: !277, file: !16, line: 18, column: 17)
!277 = distinct !DILexicalBlock(scope: !258, file: !16, line: 18, column: 7)
!278 = !DILocation(line: 17, column: 71, scope: !258)
!279 = !DILocation(line: 18, column: 17, scope: !276)
!280 = !DILocation(line: 17, column: 77, scope: !258)
!281 = !DILocation(line: 0, scope: !277)
!282 = !DILocation(line: 18, column: 7, scope: !277)
!283 = !DILocation(line: 18, column: 29, scope: !284)
!284 = distinct !DILexicalBlock(scope: !277, file: !16, line: 18, column: 27)
!285 = !DILocation(line: 0, scope: !286)
!286 = !DILexicalBlockFile(scope: !276, discriminator: 0)
!287 = !DILocation(line: 0, scope: !276)
!288 = !DILocation(line: 19, column: 23, scope: !276)
!289 = !DILocation(line: 20, column: 12, scope: !276)
!290 = !DILocation(line: 0, scope: !291)
!291 = !DILexicalBlockFile(scope: !292, discriminator: 0)
!292 = distinct !DILexicalBlock(scope: !276, file: !16, line: 21, column: 57)
!293 = !DILocation(line: 21, column: 48, scope: !276)
!294 = !DILocation(line: 21, column: 33, scope: !276)
!295 = !DILocation(line: 0, scope: !296)
!296 = !DILexicalBlockFile(scope: !276, file: !133, discriminator: 0)
!297 = !DILocation(line: 21, column: 26, scope: !276)
!298 = !DILocation(line: 22, column: 5, scope: !276)
!299 = distinct !DISubprogram(linkageName: "objectdestroy.2", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!300 = !DILocation(line: 0, scope: !299)
!301 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_TA", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15)
!302 = !DILocation(line: 0, scope: !301)
!303 = distinct !DISubprogram(linkageName: "$s7Combine10PublishersO9ReceiveOnVy_AA12AnyPublisherVyyts5NeverOGSo17OS_dispatch_queueCGAEy_xq_GAA0F0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!304 = !DILocation(line: 0, scope: !303)
!305 = distinct !DISubprogram(name: "dismiss", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF", scope: !134, file: !16, line: 36, type: !306, scopeLine: 36, spFlags: DISPFlagDefinition, unit: !15, declaration: !308, retainedNodes: !309)
!306 = !DISubroutineType(types: !307)
!307 = !{!31, !134}
!308 = !DISubprogram(name: "dismiss", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7dismissyyF", scope: !134, file: !16, line: 36, type: !306, scopeLine: 36, spFlags: 0)
!309 = !{!310}
!310 = !DILocalVariable(name: "self", arg: 1, scope: !305, file: !16, line: 36, type: !199, flags: DIFlagArtificial)
!311 = !DILocation(line: 36, column: 8, scope: !305)
!312 = !DILocation(line: 0, scope: !305)
!313 = !DILocation(line: 36, column: 34, scope: !305)
!314 = !DILocation(line: 36, column: 38, scope: !305)
!315 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_", scope: !134, file: !16, line: 21, type: !316, scopeLine: 21, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, declaration: !318, retainedNodes: !319)
!316 = !DISubroutineType(types: !317)
!317 = !{!31, !261}
!318 = !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_", scope: !134, file: !16, line: 21, type: !316, scopeLine: 21, spFlags: DISPFlagLocalToUnit)
!319 = !{!320}
!320 = !DILocalVariable(name: "self", arg: 1, scope: !315, file: !16, line: 21, type: !268, flags: DIFlagArtificial)
!321 = !DILocation(line: 21, column: 48, scope: !315)
!322 = !DILocation(line: 0, scope: !323)
!323 = distinct !DILexicalBlock(scope: !315, file: !16, line: 21, column: 57)
!324 = !DILocation(line: 21, column: 61, scope: !323)
!325 = !DILocation(line: 0, scope: !326)
!326 = !DILexicalBlockFile(scope: !323, discriminator: 0)
!327 = !DILocation(line: 0, scope: !328)
!328 = !DILexicalBlockFile(scope: !323, file: !133, discriminator: 0)
!329 = !DILocation(line: 21, column: 63, scope: !323)
!330 = !DILocation(line: 21, column: 73, scope: !323)
!331 = distinct !DISubprogram(name: "helper", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC6helper33_93366EAB6BF230CF2051527E7A0D12D2LLySiyycFZ", scope: !134, file: !16, line: 37, type: !332, scopeLine: 37, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, declaration: !335, retainedNodes: !336)
!332 = !DISubroutineType(types: !333)
!333 = !{!154, !334, !178}
!334 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sIeg_D", file: !16, size: 128, runtimeLang: DW_LANG_Swift, identifier: "$sIeg_D")
!335 = !DISubprogram(name: "helper", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC6helper33_93366EAB6BF230CF2051527E7A0D12D2LLySiyycFZ", scope: !134, file: !16, line: 37, type: !332, scopeLine: 37, spFlags: DISPFlagLocalToUnit)
!336 = !{!337, !339}
!337 = !DILocalVariable(name: "onDismiss", arg: 1, scope: !331, file: !16, line: 37, type: !338)
!338 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !334)
!339 = !DILocalVariable(name: "self", arg: 2, scope: !331, file: !16, line: 37, type: !340, flags: DIFlagArtificial)
!340 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !178)
!341 = !DILocation(line: 37, column: 30, scope: !331)
!342 = !DILocation(line: 37, column: 23, scope: !331)
!343 = !DILocation(line: 0, scope: !344)
!344 = !DILexicalBlockFile(scope: !331, discriminator: 0)
!345 = !DILocation(line: 0, scope: !331)
!346 = !DILocation(line: 37, column: 76, scope: !331)
!347 = distinct !DISubprogram(name: "resubscribe", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGF", scope: !134, file: !16, line: 26, type: !348, scopeLine: 26, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, declaration: !350, retainedNodes: !351)
!348 = !DISubroutineType(types: !349)
!349 = !{!31, !190, !134}
!350 = !DISubprogram(name: "resubscribe", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGF", scope: !134, file: !16, line: 26, type: !348, scopeLine: 26, spFlags: DISPFlagLocalToUnit)
!351 = !{!352, !353}
!352 = !DILocalVariable(name: "p", arg: 1, scope: !347, file: !16, line: 26, type: !189)
!353 = !DILocalVariable(name: "self", arg: 2, scope: !347, file: !16, line: 26, type: !199, flags: DIFlagArtificial)
!354 = !DILocation(line: 26, column: 28, scope: !347)
!355 = !DILocation(line: 26, column: 16, scope: !347)
!356 = !DILocation(line: 0, scope: !357)
!357 = !DILexicalBlockFile(scope: !347, discriminator: 0)
!358 = !DILocation(line: 0, scope: !347)
!359 = !DILocation(line: 27, column: 7, scope: !347)
!360 = !DILocation(line: 27, column: 9, scope: !347)
!361 = !DILocation(line: 0, scope: !362)
!362 = !DILexicalBlockFile(scope: !347, file: !133, discriminator: 0)
!363 = !DILocation(line: 28, column: 10, scope: !347)
!364 = !DILocation(line: 28, column: 12, scope: !347)
!365 = !DILocation(line: 28, column: 24, scope: !347)
!366 = !DILocation(line: 28, column: 46, scope: !347)
!367 = !DILocation(line: 28, column: 66, scope: !347)
!368 = !DILocation(line: 28, column: 80, scope: !347)
!369 = !DILocation(line: 28, column: 54, scope: !347)
!370 = !DILocation(line: 0, scope: !371)
!371 = !DILexicalBlockFile(scope: !372, discriminator: 0)
!372 = distinct !DILexicalBlock(scope: !347, file: !16, line: 30, column: 9)
!373 = !DILocation(line: 29, column: 21, scope: !347)
!374 = !DILocation(line: 29, column: 8, scope: !347)
!375 = !DILocation(line: 28, column: 8, scope: !347)
!376 = !DILocation(line: 34, column: 3, scope: !347)
!377 = distinct !DISubprogram(name: "dropFirst", linkageName: "$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiFfA_", scope: !378, file: !133, type: !379, spFlags: DISPFlagDefinition, unit: !15, declaration: !381, retainedNodes: !382)
!378 = !DICompositeType(tag: DW_TAG_structure_type, name: "Publisher", scope: !22, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$s7Combine9Publisher_pD")
!379 = !DISubroutineType(types: !380)
!380 = !{!154}
!381 = !DISubprogram(name: "dropFirst", linkageName: "$s7Combine9PublisherPAAE9dropFirstyAA10PublishersO4DropVy_xGSiFfA_", scope: !378, file: !133, type: !379, spFlags: 0)
!382 = !{!383}
!383 = !DILocalVariable(name: "$\CF\84_0_0", scope: !377, file: !16, type: !384, flags: DIFlagArtificial)
!384 = !DIDerivedType(tag: DW_TAG_typedef, name: "Self", scope: !385, file: !16, baseType: !386)
!385 = !DIModule(scope: null, name: "Builtin", configMacros: "\22-DDEBUG=1\22")
!386 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "$sBpD", baseType: null, size: 64)
!387 = !DILocation(line: 0, scope: !377)
!388 = distinct !DISubprogram(linkageName: "$s7Combine12AnyPublisherVySbs5NeverOGACyxq_GAA0C0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!389 = !DILocation(line: 0, scope: !388)
!390 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFS2bcfU_", scope: !347, file: !16, line: 28, type: !391, scopeLine: 28, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, retainedNodes: !393)
!391 = !DISubroutineType(types: !392)
!392 = !{!23, !23}
!393 = !{!394}
!394 = !DILocalVariable(name: "$0", arg: 1, scope: !390, file: !16, line: 28, type: !395)
!395 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !18)
!396 = !DILocation(line: 28, column: 31, scope: !390)
!397 = !DILocation(line: 0, scope: !398)
!398 = !DILexicalBlockFile(scope: !390, discriminator: 0)
!399 = !DILocation(line: 0, scope: !390)
!400 = !DILocation(line: 28, column: 36, scope: !390)
!401 = !DILocation(line: 28, column: 44, scope: !390)
!402 = distinct !DISubprogram(linkageName: "$s7Combine10PublishersO4DropVy_AA12AnyPublisherVySbs5NeverOGGAEy_xGAA0E0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!403 = !DILocation(line: 0, scope: !402)
!404 = distinct !DISubprogram(linkageName: "$s7Combine10PublishersO6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGAEy_xGAA0F0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!405 = !DILocation(line: 0, scope: !404)
!406 = distinct !DISubprogram(linkageName: "$s7Combine10PublishersO5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGAEy_xGAA0G0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!407 = !DILocation(line: 0, scope: !406)
!408 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_", scope: !347, file: !16, line: 29, type: !409, scopeLine: 29, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15, retainedNodes: !411)
!409 = !DISubroutineType(types: !410)
!410 = !{!31, !23, !261}
!411 = !{!412, !414, !415}
!412 = !DILocalVariable(name: "_0", arg: 1, scope: !408, file: !16, line: 29, type: !413)
!413 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !28)
!414 = !DILocalVariable(name: "self", arg: 2, scope: !408, file: !16, line: 29, type: !268, flags: DIFlagArtificial)
!415 = !DILocalVariable(name: "self", scope: !416, file: !16, line: 30, type: !199)
!416 = distinct !DILexicalBlock(scope: !417, file: !16, line: 30, column: 19)
!417 = distinct !DILexicalBlock(scope: !408, file: !16, line: 30, column: 9)
!418 = !DILocation(line: 29, column: 27, scope: !408)
!419 = !DILocation(line: 29, column: 21, scope: !408)
!420 = !DILocation(line: 30, column: 19, scope: !416)
!421 = !DILocation(line: 0, scope: !422)
!422 = !DILexicalBlockFile(scope: !408, discriminator: 0)
!423 = !DILocation(line: 0, scope: !408)
!424 = !DILocation(line: 0, scope: !417)
!425 = !DILocation(line: 30, column: 9, scope: !417)
!426 = !DILocation(line: 0, scope: !427)
!427 = !DILexicalBlockFile(scope: !416, discriminator: 0)
!428 = !DILocation(line: 0, scope: !416)
!429 = !DILocation(line: 30, column: 30, scope: !416)
!430 = !DILocation(line: 31, column: 9, scope: !416)
!431 = !DILocation(line: 32, column: 14, scope: !416)
!432 = !DILocation(line: 33, column: 7, scope: !416)
!433 = !DILocation(line: 30, column: 48, scope: !434)
!434 = distinct !DILexicalBlock(scope: !417, file: !16, line: 30, column: 46)
!435 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_TA", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15)
!436 = !DILocation(line: 0, scope: !435)
!437 = distinct !DISubprogram(linkageName: "$s7Combine10PublishersO9ReceiveOnVy_AC5FirstVy_AC6FilterVy_AC4DropVy_AA12AnyPublisherVySbs5NeverOGGGGSo17OS_dispatch_queueCGAEy_xq_GAA0I0AAWl", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!438 = !DILocation(line: 0, scope: !437)
!439 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC11resubscribe33_93366EAB6BF230CF2051527E7A0D12D2LLyy7Combine12AnyPublisherVySbs5NeverOGFySbcfU0_SSyXEfu_", scope: !347, file: !133, type: !440, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15)
!440 = !DISubroutineType(types: !441)
!441 = !{!442}
!442 = !DICompositeType(tag: DW_TAG_structure_type, name: "String", scope: !25, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSSD")
!443 = !DILocation(line: 0, scope: !439)
!444 = distinct !DISubprogram(name: "RcLogInfo0", linkageName: "$s10RcExtraLog0A4Info_4file8function4lineySSyXK_s12StaticStringVAGs5Int32VtF", scope: !40, file: !133, type: !445, spFlags: DISPFlagDefinition, unit: !15)
!445 = !DISubroutineType(types: !446)
!446 = !{!31, !447, !448, !448, !449}
!447 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sSSIgo_D", flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sSSIgo_D")
!448 = !DICompositeType(tag: DW_TAG_structure_type, name: "StaticString", scope: !25, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$ss12StaticStringVD")
!449 = !DICompositeType(tag: DW_TAG_structure_type, name: "Int32", scope: !25, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$ss5Int32VD")
!450 = !DILocation(line: 0, scope: !444)
!451 = distinct !DISubprogram(name: "deinit", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfd", scope: !134, file: !16, line: 10, type: !452, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !15, declaration: !455, retainedNodes: !456)
!452 = !DISubroutineType(types: !453)
!453 = !{!454, !134}
!454 = !DICompositeType(tag: DW_TAG_structure_type, name: "$sBoD", scope: !385, flags: DIFlagFwdDecl, runtimeLang: DW_LANG_Swift, identifier: "$sBoD")
!455 = !DISubprogram(name: "deinit", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfd", scope: !134, file: !16, line: 10, type: !452, scopeLine: 10, spFlags: 0)
!456 = !{!457}
!457 = !DILocalVariable(name: "self", arg: 1, scope: !451, file: !16, line: 10, type: !199, flags: DIFlagArtificial)
!458 = !DILocation(line: 10, column: 13, scope: !451)
!459 = !DILocation(line: 0, scope: !451)
!460 = distinct !DISubprogram(name: "deinit", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfD", scope: !134, file: !16, line: 10, type: !306, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !15, declaration: !461, retainedNodes: !462)
!461 = !DISubprogram(name: "deinit", linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCfD", scope: !134, file: !16, line: 10, type: !306, scopeLine: 10, spFlags: 0)
!462 = !{!463}
!463 = !DILocalVariable(name: "self", arg: 1, scope: !460, file: !16, line: 10, type: !199, flags: DIFlagArtificial)
!464 = !DILocation(line: 10, column: 13, scope: !460)
!465 = !DILocation(line: 0, scope: !460)
!466 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCMa", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!467 = !DILocation(line: 0, scope: !466)
!468 = distinct !DISubprogram(linkageName: "$s7Combine14AnyCancellableCSgWOh", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!469 = !DILocation(line: 0, scope: !468)
!470 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproC7timeout1pAC39FakeFrameworkForChainedSinkBitcodeTest00jkL13ToggleTimeoutC_7Combine12AnyPublisherVySbs5NeverOGtcfcyyt_tcfU_yycfU_TA", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition, unit: !15)
!471 = !DILocation(line: 0, scope: !470)
!472 = distinct !DISubprogram(linkageName: "$s33RcChainedSinkReproSwiftBitcodeFix16ChainedSinkReproCSgWOh", scope: !34, file: !133, type: !242, flags: DIFlagArtificial, spFlags: DISPFlagDefinition, unit: !15)
!473 = !DILocation(line: 0, scope: !472)
