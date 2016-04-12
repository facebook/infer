(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging
module F = Format
module TypSet = Sil.TypSet

(** Android lifecycle types and their lifecycle methods that are called by the framework *)

(** work-in-progress list of known callback-registering method names *)
let callback_register_methods =
  let method_list = ["addCallback"; "register"; "setOnClickListener"] in
  IList.fold_left (fun set str -> StringSet.add str set) StringSet.empty method_list

let is_known_callback_register_method proc_str = StringSet.mem proc_str callback_register_methods

let on_destroy = "onDestroy"
let on_destroy_view = "onDestroyView"

(** return true if [pname] is a special lifecycle cleanup method *)
let is_destroy_method pname =
  match pname with
  | Procname.Java pname_java ->
      let method_name = Procname.java_get_method pname_java in
      string_equal method_name on_destroy || string_equal method_name on_destroy_view
  | _ ->
      false

let android_lifecycles =
  let android_content = "android.content" in
  let android_app = "android.app" in
  let fragment_lifecycle =
    ["onInflate"; "onAttach"; "onCreate"; "onCreateView"; "onViewCreated"; "onActivityCreated";
     "onViewStateRestored"; "onStart"; "onResume"; "onPause"; "onSaveInstanceState"; "onStop";
     on_destroy_view; on_destroy; "onDetach"] in
  [ (android_content,
     "ContentProvider",
     ["onCreate"]);
    (android_app,
     "Activity",
     ["onCreate"; "onStart"; "onRestoreInstanceState"; "onPostCreate"; "onResume"; "onPostResume";
      "onCreateDescription"; "onSaveInstanceState"; "onPause"; "onStop"; on_destroy]);
    (android_app,
     "Service",
     ["onCreate"; "onStart"; "onStartCommand"; "onBind"; "onUnbind"; on_destroy]);
    (android_content,
     "BroadcastReceiever",
     ["onReceive"]);
    (android_app,
     "Fragment",
     fragment_lifecycle);
    (* this is the pre-Android 3.0 Fragment type (can also be used post-3.0) *)
    ("android.support.v4.app",
     "Fragment",
     fragment_lifecycle);
  ]

let android_callbacks =
  let cb_strs = [
    ("android.accounts", "OnAccountsUpdateListener");
    ("android.animation", "Animator$AnimatorListener");
    ("android.animation", "LayoutTransition$TransitionListener");
    ("android.animation", "TimeAnimator$TimeListener");
    ("android.animation", "ValueAnimator$AnimatorUpdateListener");
    ("android.app", "ActionBar$OnMenuVisibilityListener");
    ("android.app", "ActionBar$OnNavigationListener");
    ("android.app", "ActionBar$TabListener");
    ("android.app", "Application$ActivityLifecycleCallbacks");
    ("android.app", "DatePickerDialog$OnDateSetListener");
    ("android.app", "FragmentBreadCrumbs$OnBreadCrumbClickListener");
    ("android.app", "FragmentManager$OnBackStackChangedListener");
    ("android.app", "KeyguardManager$OnKeyguardExitResult");
    ("android.app", "LoaderManager$LoaderCallbacks");
    ("android.app", "PendingIntent$OnFinished");
    ("android.app", "SearchManager$OnCancelListener");
    ("android.app", "SearchManager$OnDismissListener");
    ("android.app", "TimePickerDialog$OnTimeSetListener");
    ("android.bluetooth", "BluetoothProfile$ServiceListener");
    ("android.content", "ClipboardManager$OnPrimaryClipChangedListener");
    ("android.content", "ComponentCallbacks");
    ("android.content", "ComponentCallbacks2");
    ("android.content", "DialogInterface$OnCancelListener");
    ("android.content", "DialogInterface$OnClickListener");
    ("android.content", "DialogInterface$OnDismissListener");
    ("android.content", "DialogInterface$OnKeyListener");
    ("android.content", "DialogInterface$OnMultiChoiceClickListener");
    ("android.content", "DialogInterface$OnShowListener");
    ("android.content", "IntentSender$OnFinished");
    ("android.content", "Loader$OnLoadCanceledListener");
    ("android.content", "Loader$OnLoadCompleteListener");
    ("android.content", "SharedPreferences$OnSharedPreferenceChangeListener");
    ("android.content", "SyncStatusObserver");
    ("android.database.sqlite", "SQLiteTransactionListener");
    ("android.drm", "DrmManagerClient$OnErrorListener");
    ("android.drm", "DrmManagerClient$OnEventListener");
    ("android.drm", "DrmManagerClient$OnInfoListener");
    ("android.gesture", "GestureOverlayView$OnGestureListener");
    ("android.gesture", "GestureOverlayView$OnGesturePerformedListener");
    ("android.gesture", "GestureOverlayView$OnGesturingListener");
    ("android.graphics", "SurfaceTexture$OnFrameAvailableListener");
    ("android.hardware", "Camera$AutoFocusCallback");
    ("android.hardware", "Camera$AutoFocusMoveCallback");
    ("android.hardware", "Camera$ErrorCallback");
    ("android.hardware", "Camera$FaceDetectionListener");
    ("android.hardware", "Camera$OnZoomChangeListener");
    ("android.hardware", "Camera$PictureCallback");
    ("android.hardware", "Camera$PreviewCallback");
    ("android.hardware", "Camera$ShutterCallback");
    ("android.hardware", "SensorEventListener");
    ("android.hardware.display", "DisplayManager$DisplayListener");
    ("android.hardware.input", "InputManager$InputDeviceListener");
    ("android.inputmethodservice", "KeyboardView$OnKeyboardActionListener");
    ("android.location", "GpsStatus$Listener");
    ("android.location", "GpsStatus$NmeaListener");
    ("android.location", "LocationListener");
    ("android.media", "AudioManager$OnAudioFocusChangeListener");
    ("android.media", "AudioRecord$OnRecordPositionUpdateListener");
    ("android.media", "JetPlayer$OnJetEventListener");
    ("android.media", "MediaPlayer$OnBufferingUpdateListener");
    ("android.media", "MediaPlayer$OnCompletionListener");
    ("android.media", "MediaPlayer$OnErrorListener");
    ("android.media", "MediaPlayer$OnInfoListener");
    ("android.media", "MediaPlayer$OnPreparedListener");
    ("android.media", "MediaPlayer$OnSeekCompleteListener");
    ("android.media", "MediaPlayer$OnTimedTextListener");
    ("android.media", "MediaPlayer$OnVideoSizeChangedListener");
    ("android.media", "MediaRecorder$OnErrorListener");
    ("android.media", "MediaRecorder$OnInfoListener");
    ("android.media", "MediaScannerConnection$MediaScannerConnectionClient");
    ("android.media", "MediaScannerConnection$OnScanCompletedListener");
    ("android.media", "SoundPool$OnLoadCompleteListener");
    ("android.media.audiofx", "AudioEffect$OnControlStatusChangeListener");
    ("android.media.audiofx", "AudioEffect$OnEnableStatusChangeListener");
    ("android.media.audiofx", "BassBoost$OnParameterChangeListener");
    ("android.media.audiofx", "EnvironmentalReverb$OnParameterChangeListener");
    ("android.media.audiofx", "Equalizer$OnParameterChangeListener");
    ("android.media.audiofx", "PresetReverb$OnParameterChangeListener");
    ("android.media.audiofx", "Virtualizer$OnParameterChangeListener");
    ("android.media.audiofx", "Visualizer$OnDataCaptureListener");
    ("android.media.effect", "EffectUpdateListener");
    ("android.net.nsd", "NsdManager$DiscoveryListener");
    ("android.net.nsd", "NsdManager$RegistrationListener");
    ("android.net.nsd", "NsdManager$ResolveListener");
    ("android.net.sip", "SipRegistrationListener");
    ("android.net.wifi.p2p", "WifiP2pManager$ActionListener");
    ("android.net.wifi.p2p", "WifiP2pManager$ChannelListener");
    ("android.net.wifi.p2p", "WifiP2pManager$ConnectionInfoListener");
    ("android.net.wifi.p2p", "WifiP2pManager$DnsSdServiceResponseListener");
    ("android.net.wifi.p2p", "WifiP2pManager$DnsSdTxtRecordListener");
    ("android.net.wifi.p2p", "WifiP2pManager$GroupInfoListener");
    ("android.net.wifi.p2p", "WifiP2pManager$PeerListListener");
    ("android.net.wifi.p2p", "WifiP2pManager$ServiceResponseListener");
    ("android.net.wifi.p2p", "WifiP2pManager$UpnpServiceResponseListener");
    ("android.os", "CancellationSignal$OnCancelListener");
    ("android.os", "IBinder$DeathRecipient");
    ("android.os", "MessageQueue$IdleHandler");
    ("android.os", "RecoverySystem$ProgressListener");
    ("android.preference", "Preference$OnPreferenceChangeListener");
    ("android.preference", "Preference$OnPreferenceClickListener");
    ("android.preference", "PreferenceFragment$OnPreferenceStartFragmentCallback");
    ("android.preference", "PreferenceManager$OnActivityDestroyListener");
    ("android.preference", "PreferenceManager$OnActivityResultListener");
    ("android.preference", "PreferenceManager$OnActivityStopListener");
    ("android.security", "KeyChainAliasCallback");
    ("android.speech", "RecognitionListener");
    ("android.speech.tts", "TextToSpeech$OnInitListener");
    ("android.speech.tts", "TextToSpeech$OnUtteranceCompletedListener");
    ("android.view", "ActionMode$Callback");
    ("android.view", "ActionProvider$VisibilityListener");
    ("android.view", "GestureDetector$OnDoubleTapListener");
    ("android.view", "GestureDetector$OnGestureListener");
    ("android.view", "InputQueue$Callback");
    ("android.view", "KeyEvent$Callback");
    ("android.view", "MenuItem$OnActionExpandListener");
    ("android.view", "MenuItem$OnMenuItemClickListener");
    ("android.view", "ScaleGestureDetector$OnScaleGestureListener");
    ("android.view", "SurfaceHolder$Callback");
    ("android.view", "SurfaceHolder$Callback2");
    ("android.view", "TextureView$SurfaceTextureListener");
    ("android.view", "View$OnAttachStateChangeListener");
    ("android.view", "View$OnClickListener");
    ("android.view", "View$OnCreateContextMenuListener");
    ("android.view", "View$OnDragListener");
    ("android.view", "View$OnFocusChangeListener");
    ("android.view", "View$OnGenericMotionListener");
    ("android.view", "View$OnHoverListener");
    ("android.view", "View$OnKeyListener");
    ("android.view", "View$OnLayoutChangeListener");
    ("android.view", "View$OnLongClickListener");
    ("android.view", "View$OnSystemUiVisibilityChangeListener");
    ("android.view", "View$OnTouchListener");
    ("android.view", "ViewGroup$OnHierarchyChangeListener");
    ("android.view", "ViewStub$OnInflateListener");
    ("android.view", "ViewTreeObserver$OnDrawListener");
    ("android.view", "ViewTreeObserver$OnGlobalFocusChangeListener");
    ("android.view", "ViewTreeObserver$OnGlobalLayoutListener");
    ("android.view", "ViewTreeObserver$OnPreDrawListener");
    ("android.view", "ViewTreeObserver$OnScrollChangedListener");
    ("android.view", "ViewTreeObserver$OnTouchModeChangeListener");
    ("android.view.accessibility", "AccessibilityManager$AccessibilityStateChangeListener");
    ("android.view.animation", "Animation$AnimationListener");
    ("android.view.inputmethod", "InputMethod$SessionCallback");
    ("android.view.inputmethod", "InputMethodSession$EventCallback");
    ("android.view.textservice", "SpellCheckerSession$SpellCheckerSessionListener");
    ("android.webkit", "DownloadListener");
    ("android.widget", "AbsListView$MultiChoiceModeListener");
    ("android.widget", "AbsListView$OnScrollListener");
    ("android.widget", "AbsListView$RecyclerListener");
    ("android.widget", "AdapterView$OnItemClickListener");
    ("android.widget", "AdapterView$OnItemLongClickListener");
    ("android.widget", "AdapterView$OnItemSelectedListener");
    ("android.widget", "AutoCompleteTextView$OnDismissListener");
    ("android.widget", "CalendarView$OnDateChangeListener");
    ("android.widget", "Chronometer$OnChronometerTickListener");
    ("android.widget", "CompoundButton$OnCheckedChangeListener");
    ("android.widget", "DatePicker$OnDateChangedListener");
    ("android.widget", "ExpandableListView$OnChildClickListener");
    ("android.widget", "ExpandableListView$OnGroupClickListener");
    ("android.widget", "ExpandableListView$OnGroupCollapseListener");
    ("android.widget", "ExpandableListView$OnGroupExpandListener");
    ("android.widget", "Filter$FilterListener");
    ("android.widget", "NumberPicker$OnScrollListener");
    ("android.widget", "NumberPicker$OnValueChangeListener");
    ("android.widget", "NumberPicker$OnDismissListener");
    ("android.widget", "PopupMenu$OnMenuItemClickListener");
    ("android.widget", "PopupWindow$OnDismissListener");
    ("android.widget", "RadioGroup$OnCheckedChangeListener");
    ("android.widget", "RatingBar$OnRatingBarChangeListener");
    ("android.widget", "SearchView$OnCloseListener");
    ("android.widget", "SearchView$OnQueryTextListener");
    ("android.widget", "SearchView$OnSuggestionListener");
    ("android.widget", "SeekBar$OnSeekBarChangeListener");
    ("android.widget", "ShareActionProvider$OnShareTargetSelectedListener");
    ("android.widget", "SlidingDrawer$OnDrawerCloseListener");
    ("android.widget", "SlidingDrawer$OnDrawerOpenListener");
    ("android.widget", "SlidingDrawer$OnDrawerScrollListener");
    ("android.widget", "TabHost$OnTabChangeListener");
    ("android.widget", "TextView$OnEditorActionListener");
    ("android.widget", "TimePicker$OnTimeChangedListener");
    ("android.widget", "ZoomButtonsController$OnZoomListener");
  ] in
  IList.fold_left (fun cbSet (pkg, clazz) ->
      let qualified_name = Mangled.from_string (pkg ^ "." ^ clazz) in
      Mangled.MangledSet.add qualified_name cbSet) Mangled.MangledSet.empty cb_strs

(** return the complete set of superclasses of [typ *)
(* TODO (t4644852): factor out subtyping functions into some sort of JavaUtil module *)
let get_all_supertypes typ tenv =
  let get_direct_supers = function
    | Sil.Tstruct { Sil.csu = Csu.Class _; superclasses } ->
        superclasses
    | _ -> [] in
  let rec add_typ class_name typs =
    match Tenv.lookup tenv class_name with
    | Some struct_typ ->
        let typ' = Sil.Tstruct struct_typ in
        get_supers_rec typ' (TypSet.add typ' typs)
    | None -> typs
  and get_supers_rec typ all_supers =
    let direct_supers = get_direct_supers typ in
    IList.fold_left
      (fun typs class_name -> add_typ class_name typs)
      all_supers direct_supers in
  get_supers_rec typ (TypSet.add typ TypSet.empty)

(** return true if [typ0] <: [typ1] *)
let is_subtype (typ0 : Sil.typ) (typ1 : Sil.typ) tenv =
  TypSet.mem typ1 (get_all_supertypes typ0 tenv)

let is_subtype_package_class typ package classname tenv =
  let classname = Mangled.from_package_class package classname in
  match Tenv.lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, classname)) with
  | Some found_struct_typ -> is_subtype typ (Sil.Tstruct found_struct_typ) tenv
  | _ -> false

let is_context typ tenv =
  is_subtype_package_class typ "android.content" "Context" tenv

let is_application typ tenv =
  is_subtype_package_class typ "android.app" "Application" tenv

let is_activity typ tenv =
  is_subtype_package_class typ "android.app" "Activity" tenv

let is_view typ tenv =
  is_subtype_package_class typ "android.view" "View" tenv

let is_fragment typ tenv =
  is_subtype_package_class typ "android.app" "Fragment" tenv ||
  is_subtype_package_class typ "android.support.v4.app" "Fragment" tenv

(** return true if [class_name] is a known callback class name *)
let is_callback_class_name class_name = Mangled.MangledSet.mem class_name android_callbacks

(** return true if [typ] is a known callback class *)
let is_callback_class typ tenv =
  let supertyps = get_all_supertypes typ tenv in
  TypSet.exists (fun typ -> match typ with
      | Sil.Tstruct { Sil.csu = Csu.Class _; struct_name = Some classname } ->
          is_callback_class_name classname
      | _ -> false) supertyps

(** return true if [typ] is a subclass of [lifecycle_typ] *)
let typ_is_lifecycle_typ typ lifecycle_typ tenv =
  let supers = get_all_supertypes typ tenv in
  TypSet.mem lifecycle_typ supers

(** return true if [class_name] is the name of a class that belong to the Android framework *)
let is_android_lib_class class_name =
  let class_str = Typename.name class_name in
  string_is_prefix "android" class_str || string_is_prefix "com.android" class_str

(** returns an option containing the var name and type of a callback registered by [procname],
    None if no callback is registered *)
let get_callback_registered_by (pname_java : Procname.java) args tenv =
  (* TODO (t4565077): this check should be replaced with a membership check in a hardcoded list of
   * Android callback registration methods *)
  (* for now, we assume a method is a callback registration method if it is a setter and has a
   * callback class as a non - receiver argument *)
  let is_callback_register_like =
    let has_non_this_callback_arg args = IList.length args > 1 in
    let has_registery_name () =
      PatternMatch.is_setter pname_java ||
      is_known_callback_register_method (Procname.java_get_method pname_java) in
    has_registery_name () &&
    has_non_this_callback_arg args in
  let is_ptr_to_callback_class typ tenv = match typ with
    | Sil.Tptr (typ, Sil.Pk_pointer) -> is_callback_class typ tenv
    | _ -> false in
  if is_callback_register_like then
    (* we don't want to check if the receiver is a callback class; it's one of the method arguments
     * that's being registered as a callback *)
    let get_non_this_args args = IList.tl args in
    try
      Some (IList.find (fun (_, typ) -> is_ptr_to_callback_class typ tenv) (get_non_this_args args))
    with Not_found -> None
  else None

(** return a list of typ's corresponding to callback classes registered by [procdesc] *)
let get_callbacks_registered_by_proc procdesc tenv =
  let collect_callback_typs callback_typs _ instr = match instr with
    | Sil.Call ([], Sil.Const (Sil.Cfun callee), args, _, _) ->
        begin
          match callee with
          | Procname.Java callee_java ->
              begin
                match get_callback_registered_by callee_java args tenv with
                | Some (_, callback_typ) -> callback_typ :: callback_typs
                | None -> callback_typs
              end
          | _ ->
              callback_typs
        end
    | _ -> callback_typs in
  Cfg.Procdesc.fold_instrs collect_callback_typs [] procdesc

(** given an Android framework type mangled string [lifecycle_typ] (e.g., android.app.Activity) and
    a list of method names [lifecycle_procs_strs], get the appropriate typ and procnames *)
let get_lifecycle_for_framework_typ_opt lifecycle_typ lifecycle_proc_strs tenv =
  match Tenv.lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, lifecycle_typ)) with
  | Some ({ Sil.csu = Csu.Class _; struct_name = Some _; def_methods } as lifecycle_typ) ->
      (* TODO (t4645631): collect the procedures for which is_java is returning false *)
      let lookup_proc lifecycle_proc =
        IList.find (fun decl_proc ->
            match decl_proc with
            | Procname.Java decl_proc_java ->
                lifecycle_proc = Procname.java_get_method decl_proc_java
            | _ ->
                false
          ) def_methods in
      (* convert each of the framework lifecycle proc strings to a lifecycle method procname *)
      let lifecycle_procs =
        IList.fold_left (fun lifecycle_procs lifecycle_proc_str ->
            try (lookup_proc lifecycle_proc_str) :: lifecycle_procs
            with Not_found -> lifecycle_procs)
          [] lifecycle_proc_strs in
      Some (Sil.Tstruct lifecycle_typ, lifecycle_procs)
  | _ -> None

(** return the complete list of (package, lifecycle_classname, lifecycle_methods) trios *)
let get_lifecycles = android_lifecycles


let is_subclass tenv cn1 classname_str =
  let typename =
    Typename.Java.from_string classname_str in
  let lookup = Tenv.lookup tenv in
  match lookup cn1, lookup typename with
  | Some typ1, Some typ2 ->
      is_subtype (Sil.Tstruct typ1) (Sil.Tstruct typ2) tenv
  | _ -> false


(** Checks if the exception is an uncheched exception *)
let is_runtime_exception tenv typename =
  is_subclass tenv typename "java.lang.RuntimeException"


(** Checks if the class name is a Java exception *)
let is_exception tenv typename =
  is_subclass tenv typename "java.lang.Exception"


(** Checks if the class name is a Java exception *)
let is_throwable tenv typename =
  is_subclass tenv typename "java.lang.Throwable"


let non_stub_android_jar () =
  let root_dir = Filename.dirname (Filename.dirname Sys.executable_name) in
  IList.fold_left Filename.concat root_dir ["lib"; "java"; "android"; "android-19.jar"]
