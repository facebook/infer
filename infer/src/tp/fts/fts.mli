(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

(* The fts functions are provided for traversing file hierarchies.  A simple
   overview is that the fts_open() function returns a "handle" on a file
   hierarchy, which is then supplied to the other fts functions.  The function
   fts_read() returns a pointer to a structure describing one of the files in the
   file hierarchy.  The function fts_children() returns a pointer to a linked
   list of structures, each of which describes one of the files contained in a
   directory in the hierarchy.  In general, directories are visited two
   distinguishable times; in preorder (before any of their descendants are
   visited) and in postorder (after all of their descendants have been visited).
   Files are visited once.  It is possible to walk the hierarchy "logically"
   (ignoring symbolic links) or physically (visiting symbolic links), order the
   walk of the hierarchy or prune and/or revisit portions of the hierarchy.  *)

type fts_info =
    (* A directory being visited in preorder. *)
    FTS_D

  (* A directory that causes a cycle in the tree.  (The fts_cycle field of
     the FTSENT structure will be filled in as well.) *)
  | FTS_DC

  (* Any FTSENT structure that represents a file type not explicitly
     described by one of the other fts_info values. *)
  | FTS_DEFAULT

  (* A directory which cannot be read.  This is an error return, and the
     fts_errno field will be set to indicate what caused the error. *)
  | FTS_DNR

  (* A file named "."  or ".."  which was not specified as a filename to
     fts_open() (see FTS_SEEDOT). *)
  | FTS_DOT

  (* A directory being visited in postorder.  The contents of the FTSENT
     structure will be unchanged from when it was returned in preorder, that
     is, with the fts_info field set to FTS_D. *)
  | FTS_DP

  (* This is an error return, and the fts_errno field will be set to
     indicate what caused the error. *)
  | FTS_ERR

  (* A regular file. *)
  | FTS_F

  (* A file for which no stat(2) information was available.  The contents of
     the fts_statp field are undefined.  This is an error return, and the
     fts_errno field will be set to indicate what caused the error. *)
  | FTS_NS

  (* A file for which no stat(2) information was requested.  The contents of
     the fts_statp field are undefined. *)
  | FTS_NSOK

  (* A symbolic link. *)
  | FTS_SL

  (* A symbolic link with a nonexistent target.  The contents of the
     fts_statp field reference the file characteristic information for the
     symbolic link itself. *)
  | FTS_SLNONE

module FTSENT :
sig
  type t

  (* flags for FTSENT structure *)
  val info : t -> fts_info

  (* A path for accessing the file from the current directory. *)
  val accpath : t -> string

  (* The path for the file relative to the root of the traversal.  This path
     contains the path specified to fts_open() as a prefix. *)
  val path : t -> string

  (* The name of the file. *)
  val name : t -> string

  (* The depth of the traversal, numbered from -1 to N, where this file was
     found.  The FTSENT structure representing the parent of the starting point
     (or root) of the traversal is numbered -1, and the FTSENT structure for
     the root itself is numbered 0. *)
  val level : t -> int

  (* Upon return of a FTSENT structure from the fts_children() or fts_read()
     functions, with its fts_info field set to FTS_DNR, FTS_ERR or FTS_NS,
     the fts_errno field contains the value of the external variable errno
     specifying the cause of the error.  Otherwise, the contents of the
     fts_errno field are undefined. *)
  val errno : t -> int

  (* This field is provided for the use of the application program and is
     not modified by the fts functions.  It is initialized to 0. *)
  val number : t -> int
  val set_number : t -> int -> unit

  (* This field is provided for the use of the application program and is
     not modified by the fts functions.  It is initialized to NULL. *)
  val pointer : t -> unit ptr
  val set_pointer : t -> unit ptr -> unit

  (* A pointer to the FTSENT structure referencing the file in the hierarchy
     immediately above the current file, that is, the directory of which
     this file is a member.  A parent structure for the initial entry point
     is provided as well, however, only the fts_level, fts_number and
     fts_pointer fields are guaranteed to be initialized. *)
  val parent : t -> t

  (* Upon return from the fts_children() function, the fts_link field points
     to the next structure in the NULL-terminated linked list of directory
     members.  Otherwise, the contents of the fts_link field are undefined. *)
  val link : t -> t

  (* If a directory causes a cycle in the hierarchy (see FTS_DC), either
     because of a hard link between two directories, or a symbolic link
     pointing to a directory, the fts_cycle field of the structure will
     point to the FTSENT structure in the hierarchy that references the same
     file as the current FTSENT structure.  Otherwise, the contents of the
     fts_cycle field are undefined. *)
  val cycle : t -> t

  (* A pointer to stat(2) information for the file. *)
  (* val statp : t -> stat *)
end

module FTS :
sig
  type t

  val cur : t -> FTSENT.t

  val child : t -> FTSENT.t

  val array : t -> FTSENT.t list

  val dev : t -> PosixTypes.dev_t

  val path : t -> string

  val rfd : t -> int
end

type fts_open_option =
  (* This option causes any symbolic link specified as a root path to be
     followed immediately whether or not FTS_LOGICAL is also specified. *)
    FTS_COMFOLLOW

  (* This option causes the fts routines to return FTSENT structures for the
     targets of symbolic links instead of the symbolic links themselves.  If
     this option is set, the only symbolic links for which FTSENT structures
     are returned to the application are those referencing nonexistent
     files.  Either FTS_LOGICAL or FTS_PHYSICAL must be provided to the
     fts_open() function. *)
  | FTS_LOGICAL

  (* As a performance optimization, the fts functions change directories as
     they walk the file hierarchy.  This has the side-effect that an
     application cannot rely on being in any particular directory during the
     traversal.  The FTS_NOCHDIR option turns off this optimization, and the
     fts functions will not change the current directory.  Note that
     applications should not themselves change their current directory and
     try to access files unless FTS_NOCHDIR is specified and absolute
     pathnames were provided as arguments to fts_open(). *)
  | FTS_NOCHDIR

  (* By default, returned FTSENT structures reference file characteristic
     information (the statp field) for each file visited.  This option
     relaxes that requirement as a performance optimization, allowing the
     fts functions to set the fts_info field to FTS_NSOK and leave the
     contents of the statp field undefined. *)
  | FTS_NOSTAT

  (* This option causes the fts routines to return FTSENT structures for
     symbolic links themselves instead of the target files they point to.  If
     this option is set, FTSENT structures for all symbolic links in the
     hierarchy are returned to the application.  Either FTS_LOGICAL or
     FTS_PHYSICAL must be provided to the fts_open() function. *)
  | FTS_PHYSICAL

  (* By default, unless they are specified as path arguments to fts_open(),
     any files named "."  or ".."  encountered in the file hierarchy are
     ignored.  This option causes the fts routines to return FTSENT structures
     for them. *)
  | FTS_SEEDOT

  (* This option prevents fts from descending into directories that have a
     different device number than the file from which the descent began. *)
  | FTS_XDEV


(* The fts_open() function takes a list of strings naming one or more
   paths which make up a logical file hierarchy to be traversed.

   There are a number of options, at least one of which (either FTS_LOGICAL
   or FTS_PHYSICAL) must be specified.

   The argument compar() specifies a user-defined function which may be used
   to order the traversal of the hierarchy.  It takes two pointers to
   pointers to FTSENT structures as arguments and should return a negative
   value, zero, or a positive value to indicate if the file referenced by
   its first argument comes before, in any order with respect to, or after,
   the file referenced by its second argument.  The fts_accpath, fts_path
   and fts_pathlen fields of the FTSENT structures may never be used in this
   comparison.  If the fts_info field is set to FTS_NS or FTS_NSOK, the
   fts_statp field may not either.  If the compar() argument is NULL, the
   directory traversal order is in the order listed in path_argv for the
   root paths, and in the order listed in the directory for everything
   else.  *)
val fts_open :
  path_argv:string list ->
  ?compar:(FTSENT.t ptr -> FTSENT.t ptr -> int) ->
  options:fts_open_option list ->
  FTS.t

(* The fts_children() function returns a pointer to an FTSENT structure
   describing the first entry in a NULL-terminated linked list of the
   files in the directory represented by the FTSENT structure most
   recently returned by fts_read().  The list is linked through the
   fts_link field of the FTSENT structure, and is ordered by the
   user-specified comparison function, if any.  Repeated calls to
   fts_children() will recreate this linked list.

   As a special case, if fts_read() has not yet been called for a hierarchy,
   fts_children() will return a pointer to the files in the logical
   directory specified to fts_open(), that is, the arguments specified to
   fts_open().  Otherwise, if the FTSENT structure most recently returned by
   fts_read() is not a directory being visited in preorder, or the directory
   does not contain any files, fts_children() returns NULL and sets errno to
   zero.  If an error occurs, fts_children() returns NULL and sets errno
   appropriately.

   The FTSENT structures returned by fts_children() may be overwritten after
   a call to fts_children(), fts_close() or fts_read() on the same file
   hierarchy stream.

   The name_only option indicates that only the names of the files are
   needed.  The contents of all the fields in the returned linked list of
   structures are undefined with the exception of the fts_name and
   fts_namelen fields.
*)
val fts_children :
  ftsp:FTS.t ->
  name_only:bool ->
  FTSENT.t

(* The fts_read() function returns a pointer to an FTSENT structure
   describing a file in the hierarchy.  Directories (that are readable and do
   not cause cycles) are visited at least twice, once in preorder and once in
   postorder.  All other files are visited at least once.  (Hard links between
   directories that do not cause cycles or symbolic links to symbolic links may
   cause files to be visited more than once, or directories more than twice.)

   The FTSENT structures returned by fts_read() may be overwritten after a
   call to fts_close() on the same file hierarchy stream, or, after a call to
   fts_read() on the same file hierarchy stream unless they represent a file of
   type directory, in which case they will not be overwritten until after a
   call to fts_read() after the FTSENT structure has been returned by the
   function fts_read() in postorder.  *)
val fts_read : FTS.t -> FTSENT.t option

type fts_set_option = 
  (* Re-visit the file; any file type may be revisited.  The next call to
     fts_read() will return the referenced file.  The fts_stat and
     fts_info fields of the structure will be reinitialized at that time,
     but no other fields will have been changed.  This option is
     meaningful only for the most recently returned file from fts_read().
     Normal use is for postorder directory visits, where it causes the
     directory to be revisited (in both preorder and postorder) as well as
     all of its descendants. *)
    FTS_AGAIN

  (* The referenced file must be a symbolic link.  If the referenced file
     is the one most recently returned by fts_read(), the next call to
     fts_read() returns the file with the fts_info and fts_statp fields
     reinitialized to reflect the target of the symbolic link instead of
     the symbolic link itself.  If the file is one of those most
     recently returned by fts_children(), the fts_info and fts_statp
     fields of the structure, when returned by fts_read(), will reflect
     the target of the symbolic link instead of the symbolic link
     itself.  In either case, if the target of the symbolic link does
     not exist the fields of the returned structure will be unchanged
     and the fts_info field will be set to FTS_SLNONE.

     If the target of the link is a directory, the preorder return, followed
     by the return of all of its descendants, followed by a postorder return,
     is done.  *)
  | FTS_FOLLOW

  (* No descendants of this file are visited.  The file may be one of
     those most recently returned by either fts_children() or
     fts_read(). *)
  | FTS_SKIP

(* The function fts_set() allows the user application to determine
   further processing for the file f of the stream ftsp.  *)
val fts_set :
  ftsp:FTS.t ->
  f:FTSENT.t ->
  options:fts_set_option list ->
  unit

(* The fts_close() function closes a file hierarchy stream ftsp and
   restores the current directory to the directory from which fts_open()
   was called to open ftsp. *)
val fts_close : FTS.t -> unit
