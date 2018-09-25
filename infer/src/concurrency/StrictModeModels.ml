(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open MethodMatcher

(* frameworks/base/core/java/android/app/backup/BackupAgent.java *)
let is_BackupAgent_method =
  call_matches "android.app.backup.BackupAgent"
    ["onRestoreFile" (* onRestoreFile(ParcelFileDescriptor,long,int,String,String,long,long) *)
    ]
  |> Staged.unstage


(* frameworks/base/core/java/android/app/DownloadManager.java *)
let is_DownloadManager_method =
  call_matches "android.app.DownloadManager" ["rename" (* rename(Context,long,String) *)
                                             ]
  |> Staged.unstage


(* frameworks/base/core/java/android/app/NotificationManager.java *)
let is_NotificationManager_method =
  call_matches "android.app.NotificationManager"
    ["notifyAsUser" (* notifyAsUser(String,int,Notification,UserHandle) *)
    ]
  |> Staged.unstage


(* frameworks/base/core/java/android/content/pm/ActivityInfo.java *)
let is_ActivityInfo_method =
  call_matches "android.content.pm.ActivityInfo" ["dump" (* dump(Printer,String,int) *)
                                                 ]
  |> Staged.unstage


(* frameworks/base/core/java/android/content/pm/ApplicationInfo.java *)
let is_ApplicationInfo_method =
  call_matches "android.content.pm.ApplicationInfo"
    [ "dump"
    ; (* dump(Printer,String,int) *)
      "getHiddenApiEnforcementPolicy"
    ; (* getHiddenApiEnforcementPolicy() *)
      "maybeUpdateHiddenApiEnforcementPolicy"
    (* maybeUpdateHiddenApiEnforcementPolicy(int,int) *)
     ]
  |> Staged.unstage


(* frameworks/base/core/java/android/content/pm/ProviderInfo.java *)
let is_ProviderInfo_method =
  call_matches "android.content.pm.ProviderInfo" ["dump" (* dump(Printer,String,int) *)
                                                 ]
  |> Staged.unstage


(* frameworks/base/core/java/android/content/pm/ResolveInfo.java *)
let is_ResolveInfo_method =
  call_matches "android.content.pm.ResolveInfo" ["dump" (* dump(Printer,String,int) *)
                                                ]
  |> Staged.unstage


(* frameworks/base/core/java/android/content/pm/ServiceInfo.java *)
let is_ServiceInfo_method =
  call_matches "android.content.pm.ServiceInfo" ["dump" (* dump(Printer,String,int) *)
                                                ]
  |> Staged.unstage


(* frameworks/base/core/java/android/database/sqlite/SQLiteDatabase.java *)
let is_SQLiteDatabase_method =
  call_matches "android.database.sqlite.SQLiteDatabase"
    [ "addCustomFunction"
    ; (* addCustomFunction(String,int,SQLiteDatabase$CustomFunction) *)
      "reopenReadWrite"
    (* reopenReadWrite() *)
     ]
  |> Staged.unstage


(* frameworks/base/core/java/android/ddm/DdmHandleHeap.java *)
let is_DdmHandleHeap_method =
  call_matches "android.ddm.DdmHandleHeap" ["handleChunk" (* handleChunk(Chunk) *)
                                           ]
  |> Staged.unstage


(* frameworks/base/core/java/android/net/Uri.java *)
let is_Uri_method =
  call_matches "android.net.Uri" ["getCanonicalUri" (* getCanonicalUri() *)
                                 ] |> Staged.unstage


(* frameworks/base/core/java/android/os/Environment.java *)
let is_Environment_method =
  call_matches "android.os.Environment"
    ["classifyExternalStorageDirectory" (* classifyExternalStorageDirectory(File) *)
    ]
  |> Staged.unstage


(* frameworks/base/core/java/android/os/Parcel.java *)
let is_Parcel_method =
  call_matches "android.os.Parcel" ["readExceptionCode" (* readExceptionCode() *)
                                   ]
  |> Staged.unstage


(* frameworks/base/core/java/android/os/RecoverySystem.java *)
let is_RecoverySystem_method =
  call_matches "android.os.RecoverySystem"
    [ "handleAftermath"
    ; (* handleAftermath(Context) *)
      "rebootPromptAndWipeUserData"
    ; (* rebootPromptAndWipeUserData(Context,String) *)
      "rebootWipeCache"
    ; (* rebootWipeCache(Context,String) *)
      "rebootWipeUserData"
    ; (* rebootWipeUserData(Context,boolean) *)
      "rebootWipeUserData"
    ; (* rebootWipeUserData(Context,boolean,String,boolean) *)
      "rebootWipeUserData"
    ; (* rebootWipeUserData(Context,boolean,String,boolean,boolean) *)
      "rebootWipeUserData"
    (* rebootWipeUserData(Context,String) *)
     ]
  |> Staged.unstage


(* frameworks/base/core/java/android/os/storage/StorageManager.java *)
let is_StorageManager_method =
  call_matches "android.os.storage.StorageManager"
    [ "getPrimaryStoragePathAndSize"
    ; (* getPrimaryStoragePathAndSize() *)
      "getPrimaryStorageSize"
    ; (* getPrimaryStorageSize() *)
      "getStorageBytesUntilLow"
    ; (* getStorageBytesUntilLow(File) *)
      "getStorageCacheBytes"
    ; (* getStorageCacheBytes(File,int) *)
      "getStorageLowBytes"
    (* getStorageLowBytes(File) *)
     ]
  |> Staged.unstage


(* frameworks/base/core/java/android/os/StrictMode.java *)
let is_StrictMode_method =
  call_matches "android.os.StrictMode"
    [ "conditionallyCheckInstanceCounts"
    ; (* conditionallyCheckInstanceCounts() *)
      "decrementExpectedActivityCount"
    ; (* decrementExpectedActivityCount(Class) *)
      "noteDiskRead"
    ; (* noteDiskRead() *)
      "noteDiskWrite"
    ; (* noteDiskWrite() *)
      "noteResourceMismatch"
    ; (* noteResourceMismatch(Object) *)
      "noteUnbufferedIO"
    ; (* noteUnbufferedIO() *)
      "queueIdle"
    (* queueIdle() *)
     ]
  |> Staged.unstage


(* frameworks/base/core/java/android/util/AtomicFile.java *)
let is_AtomicFile_method =
  call_matches "android.util.AtomicFile"
    ["getLastModifiedTime"; (* getLastModifiedTime() *) "startWrite" (* startWrite(long) *)
    ]
  |> Staged.unstage


(* frameworks/base/core/java/android/webkit/WebViewFactory.java *)
let is_WebViewFactory_method =
  call_matches "android.webkit.WebViewFactory"
    ["onWebViewProviderChanged" (* onWebViewProviderChanged(PackageInfo) *)
    ]
  |> Staged.unstage


(* frameworks/base/core/java/android/webkit/WebViewLibraryLoader.java *)
let is_WebViewLibraryLoader_method =
  call_matches "android.webkit.WebViewLibraryLoader"
    ["getWebViewNativeLibrary" (* getWebViewNativeLibrary(PackageInfo,boolean) *)
    ]
  |> Staged.unstage


(* frameworks/base/media/java/android/media/MiniThumbFile.java *)
let is_MiniThumbFile_method =
  call_matches "android.media.MiniThumbFile"
    [ "eraseMiniThumb"
    ; (* eraseMiniThumb(long) *)
      "getMagic"
    ; (* getMagic(long) *)
      "getMiniThumbFromFile"
    ; (* getMiniThumbFromFile(long,byte[]) *)
      "saveMiniThumbToFile"
    (* saveMiniThumbToFile(byte[],long,long) *)
     ]
  |> Staged.unstage


(* frameworks/base/media/java/android/media/RingtoneManager.java *)
let is_RingtoneManager_method =
  call_matches "android.media.RingtoneManager"
    ["deleteExternalRingtone" (* deleteExternalRingtone(Uri) *)
    ]
  |> Staged.unstage


(* frameworks/multidex/library/src/androidx/multidex/MultiDex.java *)
let is_MultiDex_method =
  call_matches "androidx.multidex.MultiDex"
    [ "install"
    ; (* install(Context) *)
      "installInstrumentation"
    (* installInstrumentation(Context,Context) *)
     ]
  |> Staged.unstage


(* libcore/ojluni/src/main/java/java/util/logging/FileHandler.java *)
let is_FileHandler_method =
  call_matches "java.util.logging.FileHandler" ["run" (* run() *)
                                               ] |> Staged.unstage


let is_strict_mode_violation =
  let matchers =
    [ is_BackupAgent_method
    ; is_DownloadManager_method
    ; is_NotificationManager_method
    ; is_ActivityInfo_method
    ; is_ApplicationInfo_method
    ; is_ProviderInfo_method
    ; is_ResolveInfo_method
    ; is_ServiceInfo_method
    ; is_SQLiteDatabase_method
    ; is_DdmHandleHeap_method
    ; is_Uri_method
    ; is_Environment_method
    ; is_Parcel_method
    ; is_RecoverySystem_method
    ; is_StorageManager_method
    ; is_StrictMode_method
    ; is_AtomicFile_method
    ; is_WebViewFactory_method
    ; is_WebViewLibraryLoader_method
    ; is_MiniThumbFile_method
    ; is_RingtoneManager_method
    ; is_MultiDex_method
    ; is_FileHandler_method ]
  in
  fun tenv pn actuals -> List.exists matchers ~f:(fun m -> m tenv pn actuals)
