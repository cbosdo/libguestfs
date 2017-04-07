(* virt-builder
 * Copyright (C) 2016-2017 SUSE Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

open Common_gettext.Gettext
open Common_utils
open Unix_utils
open Getopt.OptionName
open Utils
open Yajl
open Xpath_helpers

open Printf

module StringSet = Set.Make(String)

type cmdline = {
  gpg : string;
  gpgkey : string option;
  interactive : bool;
  keep_unsigned : bool;
  repo : string;
}

type disk_image_info = {
  format : string;
  size : int64;
}

let parse_cmdline () =
  let gpg = ref "gpg" in
  let gpgkey = ref "" in
  let interactive = ref false in
  let keep_unsigned = ref false in

  (* Add a --machine-readable parameter *)
  let argspec = [
    [ L"gpg" ], Getopt.Set_string ("gpg", gpg), s_"Set GPG binary/command";
    [ S 'K'; L"gpg-key" ], Getopt.Set_string ("gpgkey", gpgkey),
      s_"ID of the GPG key to sign the repo with";
    [ S 'i'; L"interactive" ], Getopt.Set interactive, s_"Ask the user about missing data";
    [ L"keep-index" ], Getopt.Set keep_unsigned, s_"Keep unsigned index";
  ] in

  let args = ref [] in
  let anon_fun s = push_front s args in
  let usage_msg =
    sprintf (f_"\
%s: create a repository for virt-builder

  virt-builder-repository REPOSITORY_PATH

A short summary of the options is given below.  For detailed help please
read the man page virt-builder-repository(1).
")
      prog in
  let opthandle = create_standard_options argspec ~anon_fun usage_msg in
  Getopt.parse opthandle;

  (* Dereference options. *)
  let args = List.rev !args in
  let gpg = !gpg in
  let gpgkey = match !gpgkey with "" -> None | s -> Some s in
  let interactive = !interactive in
  let keep_unsigned = !keep_unsigned in

  (* Check options *)
  let repo =
    match args with
    | [repo] -> repo
    | [] ->
      error (f_"virt-builder-repository /path/to/repo\nUse '/path/to/repo' to point to the repository folder.")
    | _ ->
      error (f_"too many parameters, only one path to repository is allowed") in

  {
    gpg = gpg;
    gpgkey = gpgkey;
    interactive = interactive;
    keep_unsigned = keep_unsigned;
    repo = repo;
  }

let increment_revision revision =
  match revision with
  | Utils.Rev_int n -> Utils.Rev_int (n + 1)
  | Utils.Rev_string s ->
    if Str.string_match (Str.regexp "^\\(.*[-._]\\)\\([0-9]+\\)$") s 0 then
      let prefix = Str.matched_group 1 s in
      let suffix = int_of_string (Str.matched_group 2 s) in
      Utils.Rev_string (prefix ^ (string_of_int (suffix + 1)))
    else
      Utils.Rev_string (s ^ ".1")

let osinfo_ids = ref None

let osinfo_get_short_ids () =
  match !osinfo_ids with
  | Some ids -> ids
  | None -> (
    let set = ref StringSet.empty in
    let g = open_guestfs () in

    Osinfo.read_osinfo_db g#ocaml_handle (
      fun filepath ->
        let doc = Xml.parse_file filepath in
        let xpathctx = Xml.xpath_new_context doc in
        let nodes = xpath_get_nodes xpathctx "/libosinfo/os/short-id" in
        List.iter (
          fun node ->
            let id = Xml.node_as_string node in
            set := StringSet.add id !set
        ) nodes
    );
    g#close ();
    osinfo_ids := Some (!set);
    !set
  )

let compress_to file outdir =
  let outimg = outdir // (Filename.basename file) ^ ".xz" in

  info "Compressing ...%!";
  let cmd = sprintf "cat \"%s\" | xz -f --best --block-size=16777216 - >\"%s\""
                    file outimg in
  if shell_command cmd <> 0 then
    exit 1;
  outimg

let get_disk_image_info filepath =
  let qemuimg_cmd = "qemu-img info --output json " ^ (quote filepath) in
  let lines = external_command qemuimg_cmd in
  let line = String.concat "\n" lines in
  let infos = yajl_tree_parse line in
  {
    format = object_get_string "format" infos;
    size = object_get_number "virtual-size" infos
  }

let process_image acc_entries filename repo tmprepo index interactive sigchecker =
  message (f_"Preparing %s") filename;

  let filepath = repo // filename in
  let { format = format; size = size } = get_disk_image_info filepath in
  let xz_path = compress_to filepath tmprepo in
  let checksum = Checksums.compute_checksum "sha512" xz_path in
  let compressed_size = (Unix.LargeFile.stat xz_path).Unix.LargeFile.st_size in

  let ask ?default ?values message =
    let default_str = match default with
    | None -> ""
    | Some x -> sprintf " [%s] " x in

    let list_str = match values with
    | None -> ""
    | Some x ->
      sprintf (f_"Choose one from the list below:\n %s\n")
              (String.concat "\n " x) in

    printf "%s%s" message default_str;

    let value = read_line () in
    match value with
    | "" -> default
    | s -> Some s
  in

  let rec ask_id () =
    printf (f_"Identifier: ");
    let id = read_line () in
    if not (Str.string_match (Str.regexp "[a-zA-Z0-9-_.]+") id 0) then (
      warning (f_"Allowed characters are letters, digits, - _ and .");
      ask_id ();
    ) else
      id;
  in

  let ask_arch guess =
    printf (f_"Architecture. Choose one from the list below:\n");
    let arches = ["x86_64"; "aarch64"; "armv7l"; "i686"; "ppc64"; "ppc64le"; "s390x" ] in
    iteri (
      fun i arch -> printf " [%d] %s\n" (i + 1) arch
    ) arches;

    let input = read_line () in
    if input = "exit" || input = "q" || input = "quit" then
      exit 0
    else
      try
        let i = int_of_string input in
        List.nth arches (i - 1)
      with Failure _ -> input
  in

  let ask_osinfo () =
    printf (f_ "osinfo short ID (can be found with osinfo-query os command): ");
    let value = read_line () in
    match value with
    | "" -> None
    | osinfo ->
      let osinfo_ids = osinfo_get_short_ids () in
      if not (StringSet.mem osinfo osinfo_ids) then
        warning (f_"'%s' is not a recognized osinfo OS id; using it anyway") osinfo;
      Some osinfo in

  (* Do we have an entry for that file already? *)
  let file_entry =
    try
      let xzfn filename = Filename.basename filename ^ ".xz" in
      List.hd (
        List.filter (
          fun (_, { Index.file_uri = file_uri }) ->
            (Filename.basename file_uri) = (xzfn (Filename.basename filename))
        ) index
      )
    with
    | Failure _ ->
      let entry = { Index.printable_name = None;
                    osinfo = None;
                    file_uri = "";
                    arch = "";
                    signature_uri = None;
                    checksums = None;
                    revision = Utils.Rev_int 0;
                    format = Some format;
                    size = size;
                    compressed_size = Some compressed_size;
                    expand = None;
                    lvexpand = None;
                    notes = [];
                    hidden = false;
                    aliases = None;
                    sigchecker = sigchecker;
                    proxy = Curl.UnsetProxy } in
      ("", entry) in

    let id, { Index.printable_name = printable_name;
              osinfo = osinfo;
              arch = arch;
              checksums = checksums;
              revision = revision;
              expand = expand;
              lvexpand = lvexpand;
              notes = notes;
              hidden = hidden;
              aliases = aliases } = file_entry in

    let old_checksum =
      match checksums with
      | Some csums -> (
          try
            List.find (
              fun c ->
                match c with
                | Checksums.SHA512 _ -> true
                | _ -> false
            ) csums
          with
          | _ -> Checksums.SHA512 ""
        )
      | None -> Checksums.SHA512 "" in

    message (f_"Extracting data from the image...");
    let g = new Guestfs.guestfs () in
    g#add_drive_ro filepath;

    let roots = g#inspect_os () in
    if Array.length roots = 1 then
      error (f_"virt-builder template images must have one and only one root file system");

    let root = Array.get roots 0 in
    let product = g#inspect_get_product_name root in
    let inspected_arch = g#inspect_get_arch root in
    let distro = g#inspect_get_distro root in
    let version_major = g#inspect_get_major_version root in
    let version_minor = g#inspect_get_minor_version root in
    let lvs = g#lvs () in

    g#remove_drive filepath;

    let id =
      if id = "" then (
        if interactive then ask_id ()
        else error (f_"missing image identifier");
      ) else id in

    let arch =
      if arch = "" then (
        if interactive then ask_arch inspected_arch
        else error (f_"missing architecture for %s") id;
      ) else arch in

    (* TODO error out if we have a duplicate id,arch entry in acc_entries *)

    let printable_name =
      if printable_name = None && interactive then
        ask (f_"Display name: ")
      else
        printable_name in

    let osinfo =
      if osinfo = None && interactive then
        ask_osinfo ()
      else
        osinfo in

    let expand =
      if expand = None && interactive then
        ask (f_"Expandable partition: ")
      else
        expand in

    let lvexpand =
      if lvexpand = None && interactive && lvs <> [||] then
        ask (f_"Expandable volume: ")
      else
        lvexpand in

    let revision =
      if old_checksum <> checksum then
        increment_revision revision
      else
        revision in

    (id, { Index.printable_name = printable_name;
           osinfo = osinfo;
           file_uri = Filename.basename xz_path;
           arch = arch;
           signature_uri = None;
           checksums = Some [checksum];
           revision = revision;
           format = Some format;
           size = size;
           compressed_size = Some compressed_size;
           expand = expand;
           lvexpand = lvexpand;
           notes = notes;
           hidden = hidden;
           aliases = aliases;
           sigchecker = sigchecker;
           proxy = Curl.UnsetProxy })

let has_entry id arch index =
  List.exists (
    fun (item_id, { Index.arch = item_arch }) ->
      item_id = id && item_arch = arch
  ) index


let main () =
  let cmdline = parse_cmdline () in

  (* If debugging, echo the command line arguments. *)
  debug "command line: %s\n" (String.concat " " (Array.to_list Sys.argv));

  (* Check that the paths are existing *)
  if not (Sys.file_exists cmdline.repo) then
    error (f_"repository folder '%s' doesn't exist") cmdline.repo;

  (* Create a temporary folder to work in *)
  let tmpdir = Mkdtemp.temp_dir ~base_dir:cmdline.repo
                                "virt-builder-repository." "" in
  rmdir_on_exit tmpdir;

  let tmprepo = tmpdir // "repo" in
  mkdir_p tmprepo 0o700;

  let sigchecker = Sigchecker.create ~gpg:cmdline.gpg
                                     ~check_signature:false
                                     ~gpgkey:No_Key
                                     ~tmpdir in

  let index =
    try
      let index_filename =
        List.find (
          fun filename -> Sys.file_exists (cmdline.repo // filename)
        ) [ "index.asc"; "index" ] in

      let downloader = Downloader.create ~curl:"do-not-use-curl"
                                         ~cache:None ~tmpdir in

      let source = { Sources.name = index_filename;
                     uri = cmdline.repo // index_filename;
                     gpgkey = No_Key;
                     proxy = Curl.SystemProxy;
                     format = Sources.FormatNative } in

      Index_parser.get_index ~downloader ~sigchecker ~template:true source
    with Not_found -> [] in

  (* Check for index/interactive consistency *)
  if not cmdline.interactive && index == [] then
    error (f_"the repository must contain an index file when running in automated mode");

  debug "Searching for images ...\n";

  let images =
    let is_supported_format file =
      let extension = last_part_of file '.' in
      match extension with
      | Some ext -> List.mem ext [ "qcow2"; "raw"; "img" ]
      | None -> file <> "index" in
    let files = Array.to_list (Sys.readdir cmdline.repo) in
    List.filter is_supported_format files in

  if images == [] then (
    info (f_ "No new image found");
    exit 0
  );

  info (f_ "Found new images: %s") (String.concat " " images);

  let outindex_path = tmprepo // "index" in
  let index_channel = open_out outindex_path in

  (* Generate entries for uncompressed images *)
  let images_entries = List.fold_right (
    fun filename acc ->
      let image_entry = process_image acc filename cmdline.repo tmprepo
                                      index cmdline.interactive sigchecker in
      image_entry :: acc
  ) images [] in

  (* Filter out entries for newly found images and entries
     without a corresponding image file *)
  let index = List.filter (
    fun (id, { Index.arch = arch;
               Index.file_uri = file_uri }) ->
      not (has_entry id arch images_entries) && Sys.file_exists file_uri
  ) index in

  (* Convert all URIs back to relative ones *)
  let index = List.map (
    fun (id, entry) ->
      let { Index.file_uri = file_uri } = entry in
      let rel_path =
        try
          subdirectory cmdline.repo file_uri
        with
        | Invalid_argument _ ->
          file_uri in
      let rel_entry = { entry with Index.file_uri = rel_path } in
      (id, rel_entry)
  ) index in

  (* Write all the entries *)
  List.iter (
    fun entry ->
      Index_parser.write_entry index_channel entry;
  ) (index @ images_entries);

  close_out index_channel;

  (* GPG sign the generated index *)
  (match cmdline.gpgkey with
  | None ->
    debug "Skip index signing"
  | Some gpgkey ->
    message (f_"Signing index with GPG key %s") gpgkey;
    let cmd = sprintf "%s --armor --output %s/index.gpg --export %s"
                      cmdline.gpg tmprepo gpgkey in
    if shell_command cmd <> 0 then
      error (f_"failed to export GPG key %s") gpgkey;

    let cmd = sprintf "%s --armor --default-key %s --clearsign %s/index"
                       cmdline.gpg gpgkey tmprepo in
    if shell_command cmd <> 0 then
      error (f_"failed to sign index");

    (* Remove the index file since we have the signed version of it *)
    if not cmdline.keep_unsigned then
      Sys.remove (tmprepo // "index")
  );

  message (f_"Creating index backup copy");

  List.iter (
    fun filename ->
      let filepath = cmdline.repo // filename in
      if Sys.file_exists filepath then
        do_mv filepath (filepath ^ ".bak")
  ) ["index"; "index.asc"];

  message (f_"Moving files to final destination");

  Array.iter (
    fun filename ->
      do_mv (tmprepo // filename) cmdline.repo
  ) (Sys.readdir tmprepo);

  debug "Cleanup";

  (* Remove the processed image files *)
  List.iter (
    fun filename -> Sys.remove (cmdline.repo // filename)
  ) images

let () = run_main_and_handle_errors main
