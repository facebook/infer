(***********************************************************************)
(*                                                                     *)
(*                         The CamlZip library                         *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License, with     *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Zip]: reading and writing ZIP archives *)

exception Error of string * string * string

let int64_of_uint32 n =
  Int64.(logand (of_int32 n) 0xFFFF_FFFFL)

let read1 = input_byte
let read2 ic =
  let lb = read1 ic in let hb = read1 ic in lb lor (hb lsl 8)
let read4 ic =
  let lw = read2 ic in let hw = read2 ic in
  Int32.logor (Int32.of_int lw) (Int32.shift_left (Int32.of_int hw) 16)
let read8 ic =
  let ll = read4 ic in let hl = read4 ic in
  Int64.logor (int64_of_uint32 ll) (Int64.shift_left (int64_of_uint32 hl) 32)

let readstring ic n =
  let s = Bytes.create n in
  really_input ic s 0 n; Bytes.unsafe_to_string s

let write1 = output_byte
let write2 oc n =
  write1 oc n; write1 oc (n lsr 8)
let write4 oc n =
  write2 oc (Int32.to_int n);
  write2 oc (Int32.to_int (Int32.shift_right_logical n 16))
let write8 oc n =
  write4 oc (Int64.to_int32 n);
  write4 oc (Int64.to_int32 (Int64.shift_right_logical n 32))
let writestring oc s =
  output_string oc s

type compression_method = Stored | Deflated

type entry =
  { filename: string;
    comment: string;
    methd: compression_method;
    mtime: float;
    crc: int32;
    uncompressed_size: int;
    compressed_size: int;
    is_directory: bool;
    file_offset: int64 }

type in_file =
  { if_filename: string;
    if_channel: Stdlib.in_channel;
    if_entries: entry list;
    if_directory: (string, entry) Hashtbl.t;
    if_comment: string }

let entries ifile = ifile.if_entries
let comment ifile = ifile.if_comment

type out_file =
  { of_filename: string;
    of_channel: Stdlib.out_channel;
    mutable of_entries: entry list;
    of_comment: string }

(* Return the position of the last occurrence of [pattern] in [buf],
   or -1 if not found. *)

let strrstr (pattern: string) (buf: bytes) ofs len =
  let rec search i j =
    if i < ofs then -1
    else if j >= String.length pattern then i
    else if String.get pattern j = Bytes.get buf (i + j) then search i (j+1)
    else search (i-1) 0
  in search (ofs + len - String.length pattern) 0

(* Determine if a file name is a directory (ends with /) *)

let filename_is_directory name =
  String.length name > 0 && name.[String.length name - 1] = '/'

(* Convert between Unix dates and DOS dates *)

let unixtime_of_dostime time date =
  fst(Unix.mktime
        { Unix.tm_sec = (time lsl 1) land 0x3e;
          Unix.tm_min = (time lsr 5) land 0x3f;
          Unix.tm_hour = (time lsr 11) land 0x1f;
          Unix.tm_mday = date land 0x1f;
          Unix.tm_mon = ((date lsr 5) land 0xf) - 1;
          Unix.tm_year = ((date lsr 9) land 0x7f) + 80;
          Unix.tm_wday = 0;
          Unix.tm_yday = 0;
          Unix.tm_isdst = false })

let dostime_of_unixtime t =
  let tm = Unix.localtime t in
  (tm.Unix.tm_sec lsr 1
     + (tm.Unix.tm_min lsl 5)
     + (tm.Unix.tm_hour lsl 11),
   tm.Unix.tm_mday
     + (tm.Unix.tm_mon + 1) lsl 5
     + (tm.Unix.tm_year - 80) lsl 9)

(* Parse the extra fields attached to some other structures *)

let parse_extra_field ef =
  let rec parse accu pos =
    if pos + 4 > String.length ef then List.rev accu else begin
      let id = String.get_uint16_le ef pos in
      let sz = String.get_uint16_le ef (pos + 2) in
      let sz = min sz (String.length ef - (pos + 4)) in
      let data = String.sub ef (pos + 4) sz in
      parse ((id, data) :: accu) (pos + 4 + sz)
    end
  in parse [] 0

(* Locate the end of central directory record *)

let locate_ecd filename ic =
  let buf = Bytes.create 256 in
  let filelen = LargeFile.in_channel_length ic in
  let rec find_ecd pos len =
    (* On input, bytes 0 ... len - 1 of buf reflect what is at pos in ic *)
    if pos <= 0L || Int64.sub filelen pos >= 0x10000L then
      raise (Error(filename, "",
                   "end of central directory not found, not a ZIP file"));
    let toread = if pos >= 128L then 128 else Int64.to_int pos in
    (* Make room for "toread" extra bytes, and read them *)
    Bytes.blit buf 0 buf toread (256 - toread);
    let newpos = Int64.(sub pos (of_int toread)) in
    LargeFile.seek_in ic newpos;
    really_input ic buf 0 toread;
    let newlen = min (toread + len) 256 in
    (* Search for magic number *)
    let ofs = strrstr "PK\005\006" buf 0 newlen in
    if ofs >= 0 && newlen >= 22 &&
       (let comment_len = Bytes.get_uint16_le buf (ofs + 20) in
        Int64.(add newpos (of_int (ofs + 22 + comment_len)))= filelen)
    then Int64.(add newpos (of_int ofs))
    else find_ecd newpos newlen
  in find_ecd filelen 0

(* Read ZIP64 end of central directory record locator *)

let read_ecd64_locator filename ic ecd_pos =
  if ecd_pos < 20L then
    raise(Error(filename, "", "ZIP64 ECD record locator missing"));
  let ecd64_locator_pos = Int64.(sub ecd_pos (of_int 20)) in
  LargeFile.seek_in ic ecd64_locator_pos ;
  let magic = read4 ic in
  if magic <> 0x07064b50l then
    raise(Error(filename, "", "ZIP64 ECD record locator missing"));
  let disk_no = read4 ic in
  let ecd64_offset = read8 ic in
  let n_disks = read4 ic in
  if disk_no <> 0l || n_disks <> 0l then
    raise (Error(filename, "", "multi-disk ZIP files not supported"));
  ecd64_offset

(* Read ZIP64 end of central directory record *)

type cd_info = {
  cd_offset: int64;   (* file position of start of CD *)
  cd_size: int64;     (* size of CD in bytes *)
  cd_count: int64;    (* number of CD entries *)
  ecd_comment: string
}

let read_ecd64 filename ic ecd_pos comment =
  let ecd64_pos = read_ecd64_locator filename ic ecd_pos in
  LargeFile.seek_in ic ecd64_pos ;
  let magic = read4 ic in
  if magic <> 0x06064b50l then
    raise(Error(filename, "", "ZIP64 ECD record missing"));
  let _size = read8 ic in
  let _version_made_by = read2 ic in
  let _version_needed = read2 ic in
  let n_disks = read4 ic in
  let cd_disk_no = read4 ic in
  let _disk_n_entries = read8 ic in
  let cd_count = read8 ic in
  let cd_size = read8 ic in
  let cd_offset = read8 ic in
  if cd_disk_no <> 0l || n_disks <> 0l then
    raise (Error(filename, "", "multi-disk ZIP files not supported"));
  { cd_offset; cd_size; cd_count; ecd_comment = comment }

(* Read end of central directory record *)

let read_ecd filename ic =
  let ecd_pos = locate_ecd filename ic in
  LargeFile.seek_in ic ecd_pos;
  let magic = read4 ic in
  let disk_no = read2 ic in
  let cd_disk_no = read2 ic in
  let _disk_entries = read2 ic in
  let cd_entries = read2 ic in
  let cd_size = read4 ic in
  let cd_offset = read4 ic in
  let comment_len = read2 ic in
  let comment = readstring ic comment_len in
  assert (magic = Int32.of_int 0x06054b50);
  if disk_no <> 0 || cd_disk_no <> 0 then
    raise (Error(filename, "", "multi-disk ZIP files not supported"));
  if cd_offset = 0xffff_ffffl || cd_size = 0xffff_ffffl then
    read_ecd64 filename ic ecd_pos comment
  else
    { cd_offset = int64_of_uint32 cd_offset;
      cd_size = int64_of_uint32 cd_size;
      cd_count = Int64.of_int cd_entries;
      ecd_comment = comment }

(* Fixup sizes from a ZIP64 extended information extra field *)

let fixup_sizes extra uncompressed_size compressed_size offset =
  let pos = ref 0 in
  let process orig =
    if orig <> 0xFFFF_FFFFl then
      int64_of_uint32 orig
    else begin
      let newval = String.get_int64_le extra !pos in
      pos := !pos + 8;
      newval
    end in
  let uncompressed_size = process uncompressed_size in
  let compressed_size = process compressed_size in
  let offset = process offset in
  (uncompressed_size, compressed_size, offset)

(* Read central directory entry *)

let read_directory_entry filename ic =
  let magic = read4 ic in
  if magic <> 0x02014b50l then
    raise (Error(filename, "", "wrong file header in central directory"));
  let _version_made_by = read2 ic in
  let _version_needed = read2 ic in
  let flags = read2 ic in
  let methd = read2 ic in
  let lastmod_time = read2 ic in
  let lastmod_date = read2 ic in
  let crc = read4 ic in
  let compr_size = read4 ic in
  let uncompr_size = read4 ic in
  let name_len = read2 ic in
  let extra_len = read2 ic in
  let comment_len = read2 ic in
  let _disk_number = read2 ic in
  let _internal_attr = read2 ic in
  let _external_attr = read4 ic in
  let header_offset = read4 ic in
  let name = readstring ic name_len in
  let extra = readstring ic extra_len in
  let comment = readstring ic comment_len in
  if flags land 1 <> 0 then
    raise (Error(filename, name, "encrypted entries not supported"));
  let (uncompressed_size, compressed_size, file_offset) =
    if compr_size <> 0xffff_ffffl
    && uncompr_size <> 0xffff_ffffl
    && header_offset <> 0xffff_ffffl
    then
      (int64_of_uint32 uncompr_size,
       int64_of_uint32 compr_size,
       int64_of_uint32 header_offset)
    else begin
      match List.assoc_opt 1 (parse_extra_field extra) with
      | None ->
          raise(Error(filename, name, "ZIP64 extensible data record missing"))
      | Some e ->
          fixup_sizes e uncompr_size compr_size header_offset
    end in
  let int_of_uint64 n =
    if n >= 0L && n <= Int64.of_int max_int
    then Int64.to_int n
    else raise(Error(filename, name, "size too large to be represented"))
  in
  { filename = name;
    comment = comment;
    methd = (match methd with
             | 0 -> Stored
             | 8 -> Deflated
             | _ -> raise (Error(filename, name,
                                     "unknown compression method")));
    mtime = unixtime_of_dostime lastmod_time lastmod_date;
    crc = crc;
    uncompressed_size = int_of_uint64 uncompressed_size;
    compressed_size = int_of_uint64 compressed_size;
    is_directory = filename_is_directory name;
    file_offset
  }

(* Read central directory *)

let read_cd filename ic cdinfo =
  try
    LargeFile.seek_in ic cdinfo.cd_offset;
    let entries = ref [] in
    let entrycnt = ref Int64.zero in
    let cd_bound = Int64.add cdinfo.cd_offset cdinfo.cd_size in
    while LargeFile.pos_in ic < cd_bound do
      entrycnt := Int64.(add !entrycnt one) ;
      let e = read_directory_entry filename ic in
      entries := e :: !entries
    done;
    if cd_bound <> LargeFile.pos_in ic
    || (cdinfo.cd_count <> !entrycnt && cdinfo.cd_count <> 0xFFFFL)
    then
      raise(Error(filename, "",
                  "wrong number of entries in central directory"));
    List.rev !entries
  with End_of_file ->
    raise (Error(filename, "", "end-of-file while reading central directory"))

(* Open a ZIP file for reading *)

let open_in filename =
  let ic = Stdlib.open_in_bin filename in
  try
    let cdinfo = read_ecd filename ic in
    let entries = read_cd filename ic cdinfo in
    let table_size =
      match Int64.(div cdinfo.cd_count 3L |> unsigned_to_int) with
        Some sz -> sz
      | None -> 65535 in
    let dir = Hashtbl.create table_size in
    List.iter (fun e -> Hashtbl.add dir e.filename e) entries;
    { if_filename = filename;
      if_channel = ic;
      if_entries = entries;
      if_directory = dir;
      if_comment = cdinfo.ecd_comment }
  with exn ->
    Stdlib.close_in ic; raise exn

(* Close a ZIP file opened for reading *)

let close_in ifile =
  Stdlib.close_in ifile.if_channel

(* Return the info associated with an entry *)

let find_entry ifile name =
  Hashtbl.find ifile.if_directory name

(* Position on an entry *)

let goto_entry ifile e =
  try
    let ic = ifile.if_channel in
    LargeFile.seek_in ic e.file_offset;
    let magic = read4 ic in
    if magic <> 0x04034b50l then
       raise (Error(ifile.if_filename, e.filename, "wrong local file header"));
    let _version_needed = read2 ic in
    let _flags = read2 ic in
    let _methd = read2 ic in
    let _lastmod_time = read2 ic in
    let _lastmod_date = read2 ic in
    let _crc = read4 ic in
    let _compr_size = read4 ic in
    let _uncompr_size = read4 ic in
    let filename_len = read2 ic in
    let extra_len = read2 ic in
    (* Could validate information read against directory entry, but
       what the heck *)
    LargeFile.seek_in ifile.if_channel
      (Int64.add e.file_offset (Int64.of_int (30 + filename_len + extra_len)))
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated local file header"))

(* Read the contents of an entry as a string *)

let read_entry ifile e =
  try
    goto_entry ifile e;
    let res = Bytes.create e.uncompressed_size in
    match e.methd with
      Stored ->
        if e.compressed_size <> e.uncompressed_size then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for stored entry"));
        really_input ifile.if_channel res 0 e.uncompressed_size;
        Bytes.unsafe_to_string res
    | Deflated ->
        let in_avail = ref e.compressed_size in
        let out_pos = ref 0 in
        begin try
          Zlib.uncompress ~header:false
            (fun buf ->
              let read = input ifile.if_channel buf 0
                               (min !in_avail (Bytes.length buf)) in
              in_avail := !in_avail - read;
              read)
            (fun buf len ->
              if !out_pos + len > Bytes.length res then
                raise (Error(ifile.if_filename, e.filename,
                             "wrong size for deflated entry (too much data)"));
              Bytes.blit buf 0 res !out_pos len;
              out_pos := !out_pos + len)
        with Zlib.Error(_, _) ->
          raise (Error(ifile.if_filename, e.filename, "decompression error"))
        end;
        if !out_pos <> Bytes.length res then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for deflated entry (not enough data)"));
        let crc = Zlib.update_crc Int32.zero res 0 (Bytes.length res) in
        if crc <> e.crc then
          raise (Error(ifile.if_filename, e.filename, "CRC mismatch"));
        Bytes.unsafe_to_string res
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated data"))

(* Write the contents of an entry into an out channel *)

let copy_entry_to_channel ifile e oc =
  try
    goto_entry ifile e;
    match e.methd with
      Stored ->
        if e.compressed_size <> e.uncompressed_size then
          raise (Error(ifile.if_filename, e.filename,
                       "wrong size for stored entry"));
        let buf = Bytes.create 4096 in
        let rec copy n =
          if n > 0 then begin
            let r = input ifile.if_channel buf 0 (min n (Bytes.length buf)) in
            output oc buf 0 r;
            copy (n - r)
          end in
        copy e.uncompressed_size
    | Deflated ->
        let in_avail = ref e.compressed_size in
        let crc = ref Int32.zero in
        begin try
          Zlib.uncompress ~header:false
            (fun buf ->
              let read = input ifile.if_channel buf 0
                               (min !in_avail (Bytes.length buf)) in
              in_avail := !in_avail - read;
              read)
            (fun buf len ->
              output oc buf 0 len;
              crc := Zlib.update_crc !crc buf 0 len)
        with Zlib.Error(_, _) ->
          raise (Error(ifile.if_filename, e.filename, "decompression error"))
        end;
        if !crc <> e.crc then
          raise (Error(ifile.if_filename, e.filename, "CRC mismatch"))
  with End_of_file ->
    raise (Error(ifile.if_filename, e.filename, "truncated data"))

(* Write the contents of an entry to a file *)

let copy_entry_to_file ifile e outfilename =
  let oc = open_out_bin outfilename in
  try
    copy_entry_to_channel ifile e oc;
    close_out oc;
    begin try
      Unix.utimes outfilename e.mtime e.mtime
    with Unix.Unix_error(_, _, _) | Invalid_argument _ -> ()
    end
  with x ->
    close_out oc;
    Sys.remove outfilename;
    raise x

(* Open a ZIP file for writing *)

let open_out ?(comment = "") filename =
  if String.length comment >= 0x10000 then
    raise(Error(filename, "", "comment too long"));
  { of_filename = filename;
    of_channel = Stdlib.open_out_bin filename;
    of_entries = [];
    of_comment = comment }

(* Close a ZIP file for writing.  Add central directory and ECD. *)

let write4_cautious oc ov n =
  write4 oc (if ov then 0xFFFF_FFFFl else Int64.to_int32 n)

let write_directory_entry oc e =
  let overflow =
       e.file_offset > 0xFFFF_FFFFL
    || Int64.of_int e.compressed_size > 0xFFFF_FFFFL
    || Int64.of_int e.uncompressed_size > 0xFFFF_FFFFL in
  write4 oc 0x02014b50l;                (* signature *)
  let version = match e.methd with Stored -> 10 | Deflated -> 20 in
  write2 oc version;                    (* version made by *)
  write2 oc version;                    (* version needed to extract *)
  write2 oc 8;                          (* flags *)
  write2 oc (match e.methd with Stored -> 0 | Deflated -> 8); (* method *)
  let (time, date) = dostime_of_unixtime e.mtime in
  write2 oc time;                       (* last mod time *)
  write2 oc date;                       (* last mod date *)
  write4 oc e.crc;                      (* CRC32 *)
  write4_cautious oc overflow (Int64.of_int e.compressed_size);
                                        (* compressed size *)
  write4_cautious oc overflow (Int64.of_int e.uncompressed_size);
                                        (* uncompressed size *)
  write2 oc (String.length e.filename); (* filename length *)
  write2 oc (if overflow then 28 else 0); (* extra length *)
  write2 oc (String.length e.comment);  (* comment length *)
  write2 oc 0;                          (* disk number start *)
  write2 oc 0;                          (* internal attributes *)
  write4 oc 0l;                         (* external attributes *)
  write4_cautious oc overflow e.file_offset;     (* offset of local header *)
  writestring oc e.filename;            (* filename *)
  if overflow then begin                (* extra data *)
    write2 oc 0x0001;   (* header ID *)
    write2 oc 24;       (* payload size *)
    write8 oc (Int64.of_int e.uncompressed_size);
    write8 oc (Int64.of_int e.compressed_size);
    write8 oc e.file_offset
  end;
  writestring oc e.comment              (* file comment *)

let close_out ofile =
  let oc = ofile.of_channel in
  let start_cd = LargeFile.pos_out oc in
  List.iter (write_directory_entry oc) (List.rev ofile.of_entries);
  let start_ecd = LargeFile.pos_out oc in
  let cd_size = Int64.sub start_ecd start_cd in
  let num_entries = List.length ofile.of_entries in
  let overflow =
       num_entries > 0xFFFF
    || start_cd > 0xFFFF_FFFFL
    || cd_size > 0xFFFF_FFFFL in
  if overflow then begin
    (* Write ZIP64 end of central directory record *)
    write4 oc 0x06064b50l;              (* signature *)
    write8 oc 44L;                      (* size ECD record *)
    write2 oc 45;                       (* version made *)
    write2 oc 45;                       (* version needed *)
    write4 oc 0l;                       (* disk number *)
    write4 oc 0l;                       (* CD disk number *)
    let ne = Int64.of_int num_entries in
    write8 oc ne;                       (* num disk entries *)
    write8 oc ne;                       (* num entries *)
    write8 oc cd_size;                  (* size of the CD *)
    write8 oc start_cd;                 (* start offset for CD *)
    (* Write ZIP64 end of central directory locator *)
    write4 oc 0x07064b50l;              (* signature *)
    write4 oc 0l;                       (* CD disk number *)
    write8 oc start_ecd;                (* Position of ECD record *)
    write4 oc 0l                        (* number of disks *)
  end;
  (* Write ZIP end of central directory record *)
  write4 oc 0x06054b50l;                (* signature *)
  write2 oc 0;                          (* disk number *)
  write2 oc 0;                          (* number of disk with central dir *)
  let ne = if overflow then 0xFFFF else num_entries in
  write2 oc ne;                         (* # entries in this disk *)
  write2 oc ne;                         (* # entries in central dir *)
  write4_cautious oc overflow cd_size;  (* size of central dir *)
  write4_cautious oc overflow start_cd; (* offset of central dir *)
  write2 oc (String.length ofile.of_comment); (* length of comment *)
  writestring oc ofile.of_comment;         (* comment *)
  Stdlib.close_out oc

(* Write a local file header and return the corresponding entry *)

let add_entry_header ofile comment level mtime filename =
  if level < 0 || level > 9 then
    raise(Error(ofile.of_filename, filename, "wrong compression level"));
  if String.length filename >= 0x10000 then
    raise(Error(ofile.of_filename, filename, "filename too long"));
  if String.length comment >= 0x10000 then
    raise(Error(ofile.of_filename, filename, "comment too long"));
  let oc = ofile.of_channel in
  let pos = LargeFile.pos_out oc in
  write4 oc 0x04034b50l;                (* signature *)
  let version = if level = 0 then 10 else 20 in
  write2 oc version;                    (* version needed to extract *)
  write2 oc 0;                          (* flags *)
  write2 oc (if level = 0 then 0 else 8); (* method *)
  let (time, date) = dostime_of_unixtime mtime in
  write2 oc time;                       (* last mod time *)
  write2 oc date;                       (* last mod date *)
  write4 oc 0l;                         (* CRC32 - to be filled later *)
  write4 oc 0l;                         (* compressed size - later *)
  write4 oc 0l;                         (* uncompressed size - later *)
  write2 oc (String.length filename);   (* filename length *)
  write2 oc 20;                         (* extra length *)
  writestring oc filename;              (* filename *)
  write2 oc 0x0001;                     (* extra data - header ID *)
  write2 oc 16;                         (* payload size *)
  write8 oc 0L;                         (* compressed size - later *)
  write8 oc 0L;                         (* uncompressed size - later *)
  { filename = filename;
    comment = comment;
    methd = (if level = 0 then Stored else Deflated);
    mtime = mtime;
    crc = Int32.zero;
    uncompressed_size = 0;
    compressed_size = 0;
    is_directory = filename_is_directory filename;
    file_offset = pos }

(* Write the correct sizes and CRC in the local file header
   and update the entry *)

let update_entry ofile crc compr_size uncompr_size entry =
  let csz = Int64.of_int compr_size
  and usz = Int64.of_int uncompr_size in
  let overflow = csz > 0xFFFF_FFFFL || usz > 0xFFFF_FFFFL in
  let oc = ofile.of_channel in
  let cur = LargeFile.pos_out oc in
  LargeFile.seek_out oc (Int64.add entry.file_offset 14L);
  write4 oc crc;                        (* CRC *)
  write4_cautious oc overflow csz;      (* compressed size *)
  write4_cautious oc overflow usz;      (* uncompressed size *)
  if overflow then begin
    LargeFile.seek_out oc
      Int64.(add entry.file_offset
                 (of_int (30 + String.length entry.filename + 4)));
    write8 oc csz;                        (* compressed size *)
    write8 oc usz                         (* uncompressed size *)
  end;
  LargeFile.seek_out oc cur;
  { entry with crc = crc;
               uncompressed_size = uncompr_size;
               compressed_size = compr_size }

(* Add an entry with the contents of a string *)

let add_entry data ofile ?(comment = "")
                         ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile comment level mtime name in
  let crc = Zlib.update_crc_string Int32.zero data 0 (String.length data) in
  let compr_size =
    match level with
      0 ->
        output_substring ofile.of_channel data 0 (String.length data);
        String.length data
    | _ ->
        let in_pos = ref 0 in
        let out_pos = ref 0 in
        try
          Zlib.compress ~level ~header:false
            (fun buf ->
               let n = min (String.length data - !in_pos)
                           (Bytes.length buf) in
               String.blit data !in_pos buf 0 n;
               in_pos := !in_pos + n;
               n)
            (fun buf n ->
                output ofile.of_channel buf 0 n;
                out_pos := !out_pos + n);
          !out_pos
        with Zlib.Error(_, _) ->
          raise (Error(ofile.of_filename, name, "compression error")) in
  let e' = update_entry ofile crc compr_size (String.length data) e in
  ofile.of_entries <- e' :: ofile.of_entries

(* Add an entry with the contents of an in channel *)

let copy_channel_to_entry ic ofile ?(comment = "")
                                   ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile comment level mtime name in
  let crc = ref Int32.zero in
  let (compr_size, uncompr_size) =
    match level with
      0 ->
        let buf = Bytes.create 4096 in
        let rec copy sz =
          let r = input ic buf 0 (Bytes.length buf) in
          if r = 0 then sz else begin
            crc := Zlib.update_crc !crc buf 0 r;
            output ofile.of_channel buf 0 r;
            copy (sz + r)
          end in
        let size = copy 0 in
        (size, size)
    | _ ->
        let in_pos = ref 0 in
        let out_pos = ref 0 in
        try
          Zlib.compress ~level ~header:false
            (fun buf ->
               let r = input ic buf 0 (Bytes.length buf) in
               crc := Zlib.update_crc !crc buf 0 r;
               in_pos := !in_pos + r;
               r)
            (fun buf n ->
               output ofile.of_channel buf 0 n;
               out_pos := !out_pos + n);
          (!out_pos, !in_pos)
        with Zlib.Error(_, _) ->
          raise (Error(ofile.of_filename, name, "compression error")) in
  let e' = update_entry ofile !crc compr_size uncompr_size e in
  ofile.of_entries <- e' :: ofile.of_entries

(* Add an entry with the contents of a file *)

let copy_file_to_entry infilename ofile ?(comment = "")
                                        ?(level = 6) ?mtime name =
  let ic = open_in_bin infilename in
  let mtime' =
    match mtime with
      Some t -> mtime
    | None ->
        try Some((Unix.stat infilename).Unix.st_mtime)
        with Unix.Unix_error(_,_,_) -> None in
  try
    copy_channel_to_entry ic ofile ~comment ~level ?mtime:mtime' name;
    Stdlib.close_in ic
  with x ->
    Stdlib.close_in ic; raise x


(* Add an entry whose content will be produced by the caller *)

let add_entry_generator ofile ?(comment = "")
                              ?(level = 6) ?(mtime = Unix.time()) name =
  let e = add_entry_header ofile comment level mtime name in
  let crc = ref Int32.zero in
  let compr_size = ref 0 in
  let uncompr_size = ref 0 in
  let finished = ref false in
  let check () =
    if !finished then
      raise (Error(ofile.of_filename, name, "entry already finished"))
  in
  let finish () =
    finished := true;
    let e' = update_entry ofile !crc !compr_size !uncompr_size e in
    ofile.of_entries <- e' :: ofile.of_entries
  in
  match level with
  | 0 ->
      (fun buf pos len ->
        check ();
        output ofile.of_channel buf pos len;
        compr_size := !compr_size + len;
        uncompr_size := !uncompr_size + len;
        crc := Zlib.update_crc !crc buf pos len
      ),
      (fun () ->
        check ();
        finish ()
      )
  | _ ->
      let (send, flush) = Zlib.compress_direct ~level ~header:false
          (fun buf n ->
            output ofile.of_channel buf 0 n;
            compr_size := !compr_size + n)
      in
      (fun buf pos len ->
        check ();
        try
          send buf pos len;
          uncompr_size := !uncompr_size + len;
          crc := Zlib.update_crc !crc buf pos len
        with Zlib.Error(_, _) ->
          raise (Error(ofile.of_filename, name, "compression error"))
      ),
      (fun () ->
        check ();
        try
          flush ();
          finish ()
        with Zlib.Error(_, _) ->
          raise (Error(ofile.of_filename, name, "compression error"))
      )
