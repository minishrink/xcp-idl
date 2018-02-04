(*
 * Copyright (C) 2011 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(**
 * @group Storage
*)

let service_name="storage"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)

let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)

let set_sockets_dir x =
  default_path := Filename.concat x service_name

let uri () = "file:" ^ !default_path


(** Primary key identifying the SR *)
type sr = string
[@@deriving rpcty]

(** Primary key identifying a VDI within an SR *)
type vdi = string
[@@deriving rpcty]

(** Opaque identifier used by the client to identify a particular operation *)
type debug_info = string
[@@deriving rpcty]

(** Unique user identifier *)
type uuid = string
[@@deriving rpcty]

(** The result of a successful VDI.attach: this information (eg) can be used to
    	connect a VBD backend to a VBD frontend *)
type attach_info =
  {	params          : string;
    o_direct        : bool;
    o_direct_reason : string;
    xenstore_data   : (string * string) list;
  } [@@deriving rpcty]

(** Uniquely identifies the contents of a VDI *)
type content_id = string
[@@deriving rpcty]

(** The result of an operation which creates or examines a VDI *)
type vdi_info =
  { vdi: vdi;                         [@default ""]
    uuid: string option;              [@default None]
    content_id: content_id;           [@default ""]
    name_label: string;               [@default ""]
    name_description: string;         [@default ""]
    ty: string;                       [@default "user"]
    (* sm_config: workaround via XenAPI *)
    metadata_of_pool: string;         [@default ""]
    is_a_snapshot: bool;              [@default false]
    snapshot_time: string;            [@default
    Xapi_stdext_date.Date.to_string
      Xapi_stdext_date.Date.never]
    snapshot_of: vdi;                 [@default ""]
    (* managed: workaround via XenAPI *)
    read_only: bool;                  [@default false]
    cbt_enabled: bool;                [@default false]
    (* missing: workaround via XenAPI *)
    virtual_size: int64;              [@default 0L]
    physical_utilisation: int64;      [@default 0L]
    (* xenstore_data: workaround via XenAPI *)
    persistent: bool;                 [@default false]
    sharable: bool;                   [@default false]
    sm_config: (string * string) list [@default []]
  } [@@deriving rpcty]

(*
let default_vdi_info = {
  vdi = "";
  uuid = None;
  content_id = "";
  name_label = "";
  name_description = "";
  ty = "user";
  metadata_of_pool = "";
  is_a_snapshot = false;
  snapshot_time = Xapi_stdext_date.Date.to_string Xapi_stdext_date.Date.never;
  snapshot_of = "";
  read_only = false;
  cbt_enabled = false;
  virtual_size = 0L;
  physical_utilisation = 0L;
  persistent = true;
  sharable = false;
  sm_config = [];
} *)

(* let vdi_info_of_rpc rpc = Rpc.struct_extend rpc (rpc_of_vdi_info default_vdi_info) |> vdi_info_of_rpc *)

type sr_health = Healthy | Recovering
[@@deriving rpcty]

(** SR metadata properties *)
type sr_info =
  { name_label       : string;
    (** title string given to SR *)
    name_description : string;
    (** description string of SR *)
    total_space      : int64;
    (** total number of bytes on the storage substrate *)
    free_space       : int64;
    (** current free space on the storage substrate *)
    clustered        : bool;
    (** is this SR in a cluster *)
    health: sr_health
    (** is this SR healthy or recovering *)
  } [@@deriving rpcty]

(* let string_of_vdi_info (x: vdi_info) = Jsonrpc.to_string (rpc_of_vdi_info x) *)

(** Each VDI is associated with one or more "attached" or "activated" "datapaths". *)
type dp = string
[@@deriving rpcty]

type dp_stat_t =
  {	superstate: Vdi_automaton.state;
    dps:        (string * Vdi_automaton.state) list;
  } [@@deriving rpcty]

(* let string_of_dp_stat_t (x: dp_stat_t) = Jsonrpc.to_string (rpc_of_dp_stat_t x) *)

type probe =
  { srs:  (string * sr_info) list; (** SRs we found *)
    uris: string list;             (** other uris we found which could be probed recursively *)
  } [@@deriving rpcty]

type probe_result =
  | Raw of string (** SMAPIv1 adapters return arbitrary data *)
  | Probe of probe
[@@deriving rpcty]

module Mirror = struct
  type id = string
  [@@deriving rpcty]

  type state =
    | Receiving
    | Sending
    | Copying
  [@@deriving rpcty]

  type t =
    {	source_vdi : vdi;
      dest_vdi   : vdi;
      state      : state list;
      failed     : bool
    } [@@deriving rpcty]

  type mirror_receive_result_vhd_t =
    { mirror_vdi      : vdi_info;
      mirror_datapath : dp;
      copy_diffs_from : content_id option;
      copy_diffs_to   : vdi;
      dummy_vdi       : vdi
    } [@@deriving rpcty]

  type mirror_receive_result =
    | Vhd_mirror of mirror_receive_result_vhd_t
  [@@deriving rpcty]

  type similars = content_id list
  [@@deriving rpcty]
end

type async_result_t =
  | Vdi_info  of vdi_info
  | Mirror_id of Mirror.id
[@@deriving rpcty]


module Task = struct
  type id = string
  [@@deriving rpcty]

  type async_result = async_result_t
  [@@deriving rpcty]

  type completion_t =
    { duration : float;
      result   : async_result option
    } [@@deriving rpcty]

  type state =
    | Pending   of float
    | Completed of completion_t
    | Failed    of Rpc.t
  [@@deriving rpcty]

  type t =
    {	id:          id;
      dbg:         string;
      ctime:       float;
      state:       state;
      subtasks:   (string * state) list;
      debug_info: (string * string) list;
      backtrace:   string;
    } [@@deriving rpcty]
end

module Dynamic = struct
(* ID types for different storage objects *)
  type id =
    | Task   of Task.id
    | Vdi    of vdi
    | Dp     of dp
    | Mirror of Mirror.id
  [@@deriving rpcty]

  type t =
    | Task_t   of Task.id * Task.t
    | Vdi_t    of vdi * vdi_info
    | Dp_t     of dp * dp_stat_t
    | Mirror_t of Mirror.id * Mirror.t
  [@@deriving rpcty]

end

(** generic storage daemon error type *)
type storage_errors =
  | Backend_error_with_backtrace of (string * (string list))
  (** name * params *)
  | Sr_not_attached of string
  (** error: SR must be attached to access VDIs *)
  | Vdi_does_not_exist of string
  (** error: the VDI is unknown *)
  | Illegal_transition of (Vdi_automaton.state * Vdi_automaton.state)
  (** This operation implies an illegal state transition *)
  | Backend_error of (string * (string list))
  (** error: of the form SR_BACKEND_FAILURE *)
  | Does_not_exist of (string * string)
  | Cancelled of string
  | Redirect of string option
  | Sr_attached of string
  | Unimplemented of string
  | Activated_on_another_host of uuid
  | Duplicated_key of string
  | No_storage_plugin_for_sr of string
  | Content_ids_do_not_match of (string * string)
  | Missing_configuration_parameter of string
  | Storage_error of string
[@default Storage_error]
[@@deriving rpcty]

module StorageErrHandler = Error.Make(struct
    type t = storage_errors
    let t  = storage_errors
  end)
let storage_err = StorageErrHandler.error

(** Storage query result *)
type query_result =
  {	driver                 : string;
    name                   : string;
    description            : string;
    vendor                 : string;
    copyright              : string;
    version                : string;
    required_api_version   : string;
    features               : string list;
    configuration          : (string * string) list;
    required_cluster_stack : string list;
  } [@@deriving rpcty]

(** --- code generation -- *)

module RPC_API(R : RPC) = struct
  open R

  let description =
    Interface.{ name = "SMAPIv3"
              ; namespace = None
              ; description =
                  [ "This interface is used by Xapi and SMAPIv3 to interface "
                  ; "with the storage layer"]
              ; version=(1,0,0)
              }

  let implementation = implement description

  (* common API parameters*)

  type vdi_info_list = vdi_info list [@@deriving rpcty]

  let unit_p         = Param.mk Types.unit
  let string_p       = Param.mk Types.string
  let vdi_info_p     = Param.mk ~description:[ "VDI info" ] vdi_info
  let vdi_info_lst_p = Param.mk ~description:[ "List of VDI info records" ] vdi_info_list
  let name_p         = Param.mk ~name:"name" ~description:[ "Name of VDI" ] Types.string
  let url_p          = Param.mk ~name:"URL"  ~description:[ "URL of remote" ] Types.string
  let vdi_p          = Param.mk ~name:"vdi"  ~description:[ "Virtual disk image object" ] vdi
  let sr_p           = Param.mk ~name:"sr"   ~description:[ "Storage repository object" ] sr

  let dbg_p =          Param.mk ~name:"debug_info" ~description:
      [ "Uninterpreted string used for debugging" ] debug_info

  let string_param name description = Param.mk ~name ~description Types.string

  module Query = struct
    let query =
      let query_p = Param.mk ~description:[ "Result of query" ] query_result in
      declare "query"
        [ "[query ()] returns information about this storage driver" ]
        (dbg_p @-> returning query_result storage_err)

    let diagnostics = declare "diagnostics"
      [ "[diagnostics ()] returns diagnostic information about this storage driver" ]
      (dbg_p @-> returning string_p storage_err)
  end

  module DP = struct
    (** Functions which create/destroy (or register/unregister) dps *)

    let dp_p = Param.mk ~name:"dp" ~description:[ "Datapath" ] dp

    let create =
      let id_p = Param.mk ~name:"id" ~description:["ID"] Types.string in
      declare "create"
        [ "[create task id]: creates and returns a dp" ]
        (dbg_p @-> id_p @-> returning dp_p storage_err)

    let destroy =
      let allow_leak_p = Param.mk ~name:"allow leak" ~description:
          [ "is a leak allowed?" ] Types.bool in
      declare "destroy"
        [ "Frees any resources associated with [id] and destroys it."
        ;	" This will typically do any needed VDI.detach, VDI.deactivate cleanup. "]
        (dbg_p @-> dp_p @-> allow_leak_p @-> returning unit_p storage_err)

    let attach_info = declare "attach_info"
        [ "Returns the params of the dp (the return value of VDI.attach)." ]
        (dbg_p @-> sr_p @-> vdi_p @-> dp_p @-> returning attach_info storage_err)

    let diagnostics = declare "diagnostics"
      [ "[diagnostics ()]: returns a printable set of diagnostic information, "
      ; "typically including lists of all registered datapaths and their allocated"
      ; "resources." ]
      (unit_p @-> returning string_p storage_err)

    let stat_vdi = declare "stat_vdi"
      [ "Returns the state of the given VDI from the point of view of "
      ; "each dp as well as the overall superstate." ]
      (dbg_p @-> sr_p @-> vdi_p @-> unit_p @-> returning dp_stat_t storage_err)
  end

  module SR = struct

    (** Common SR API call parameters *)

    type config_lst = (string * string) list     [@@deriving rpcty]
    type vdi_pairs  = (vdi * vdi) list           [@@deriving rpcty]
    type sr_lst     = sr list                    [@@deriving rpcty]

    let make_config name desc = Param.mk ~name                  ~description config_lst

    let vdi_pairs_p           = Param.mk ~name:"snapshot_pairs" ~description:[ "List of snapshot VDI pairs" ] vdi_pairs

    let sr_strpairs_lst_p = make_config "sr"            [ "storage record" ]
    let device_conf_p     = make_config "device-config" [ "key:value pairs for device configuration" ]
    let device_conf_p     = make_config "sm-config"     [ "key:value pairs for storage management configuration" ]

    let name_label_p = string_param "name-label"       [ "Name given to SR" ]
    let name_desc_p  = string_param "name-description" [ "Description of SR" ]

    (** Functions which manipulate SRs *)

    let create =
      let phys_size_p = Param.mk ~name:"physical-size" ~description:[ "Physical size of SR" ] Types.int64 in
      declare "create"
        [ "[create dbg sr name_label name_description device_config physical_size] creates an sr with id [sr]" ]
        ( dbg_p
        @-> sr_p
        @-> name_label_p
        @-> name_desc_p
        @-> device_conf_p
        @-> phys_size_p
        @-> returning sr_strpairs_lst_p storage_err
        )

    let set_name_label = declare "set_name_label"
      [ "[set_name_label sr new_name_label] updates the name_label of SR [sr]." ]
      (dbg_p @-> sr_p @-> name_label_p @-> returning unit_p storage_err)

    let set_name_description = declare "set_name_description"
      [ "[set_name_description sr new_name_description] updates the name_description of SR [sr]." ]
      (dbg_p @-> sr_p @-> name_description_p @-> returning unit_p storage_err)

    let probe =
      let queue_p = string_param "queue" [ "Queue to search for SRs" ] in
      declare "probe"
        [ "[probe dbg queue device_config sm_config] searches on the storage device for SRs of queue [queue]" ]
        (dbg_p @-> queue_p @-> device_conf_p @-> sm_conf_p @-> returning probe_result storage_err)

    let attach = declare "attach"
      [ "[attach task sr]: attaches the SR" ]
      (dbg_p @-> sr_p @-> device_conf_p @-> returning unit_p storage_err)

    let detach = declare "detach"
      [ "[detach task sr]: detaches the SR, first detaching and/or deactivating any "
      ; "active VDIs. This may fail with Sr_not_attached, or any error from VDI.detach "
      ; "or VDI.deactivate." ]
      (dbg_p @-> sr_p @-> returning unit_p storage_err)

    let reset = declare "reset"
      [ "[reset task sr]: declares that the SR has been completely reset, e.g. by "
      ; "rebooting the VM hosting the SR backend." ]
      (dbg_p @-> sr_p @-> returning unit_p storage_err)

    let destroy = declare "destroy"
      [ "[destroy sr]: destroys (i.e. makes unattachable and unprobeable) the [sr], "
      ; " first detaching and/or deactivating any active VDIs. This may fail with "
      ; "Sr_not_attached, or any error from VDI.detach or VDI.deactivate." ]
      (dbg_p @-> sr_p @-> returning unit_p storage_err)

    let scan = declare "scan"
      [ "[scan task sr] returns a list of VDIs contained within an attached SR" ]
      (dbg_p @-> sr_p @-> returning vdi_info_list_p storage_err)

    let update_snapshot_info_src = declare "update_snapshot_info_src"
      [ "[update_snapshot_info_src sr vdi url dest dest_vdi snapshot_pairs] "
      ; "updates the fields is_a_snapshot, snapshot_time and snapshot_of "
      ; "for a list of snapshots on a remote SR." ]
      (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> url_p
        @-> dest_sr_p
        @-> dest_vdi_p
        @-> vdi_pairs_p
        @-> returning unit_p storage_err)

    let update_snapshot_info_dest = declare "update_snapshot_info_dest"
      [ "[update_snapshot_info_dest sr vdi dest src_vdi snapshot_pairs] "
      ; "updates the fields is_a_snapshot, snapshot_time and snapshot_of for a"
      ; "list of snapshots on a local SR. Typically, vdi will be a mirror of "
      ; "src_vdi, and for each item in snapshot_pairs the first will be a copy "
      ; "of the second. " ]
      (dbg_p
        @-> sr_p
        @-> vdi_p
        @-> src_vdi_p_info
        @-> vdi_pairs_p
        @-> returning unit_p storage_err)

    let stat = declare "stat"
      [ "[stat task sr] returns instantaneous SR-level statistics" ]
      (dbg_p @-> sr_p @-> returning sr_info storage_err)

    let list =
      let sr_lst_p = Param.mk ~description:[ "List of currently attached SRs" ] sr_list in
      declare "list"
        [ "[list task] returns the list of currently attached SRs" ]
        (dbg_p @-> returning sr_list_p storage_err)
  end

  module VDI = struct
    (** Functions which operate on particular VDIs.
        These functions are all idempotent from the point of view of a given [dp]. *)

    let create = declare "create"
      [ "[create task sr vdi_info] creates a new VDI in [sr] using [vdi_info]. Some "
      ; "fields in the [vdi_info] may be modified (e.g. rounded up), so the function "
      ; "returns the vdi_info which was used." ]
      (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p storage_err)

    let set_name_label = declare "set_name_label"
      [ "[set_name_label sr vdi new_name_label] updates the name_label of VDI [vdi] in SR [sr]." ]
      (dbg_p @-> sr_p @-> vdi_p @-> name_label_p @-> returning unit_p storage_err)

    let set_name_description = declare "set_name_description"
      [ "[set_name_description sr vdi new_name_description] updates the name_description of VDI [vdi] in SR [sr]." ]
      (dbg_p @-> sr_p @-> vdi_p @-> name_description_p @-> returning unit_p storage_err)

    let snapshot = declare "snapshot"
      [ "[snapshot task sr vdi_info] creates a new VDI which is a snapshot of [vdi_info] in [sr]" ]
      (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p storage_err)

    let clone = declare "clone"
      [ "[clone task sr vdi_info] creates a new VDI which is a clone of [vdi_info] in [sr]" ]
      (dbg_p @-> sr_p @-> vdi_info_p @-> returning vdi_info_p storage_err)

    let resize =
      let req_size_p = Param.mk ~name:"requested size" ~description:[ "Requested new size of VDI" ] Types.int64 in
      let new_size_p = Param.mk ~name:"new size"       ~description:[ "Actual new size of VDI" ]    Types.int64 in
      declare "resize"
        [ "[resize task sr vdi new_size] makes a VDI's virtual_size  at least [new_size] bytes. "
        ; "The function returns the new virtual_size which may be bigger (but not less than) "
        ; "requested." ]
        (dbg_p @-> sr_p @-> vdi_p @-> req_size_p @-> returning new_size_p storage_err)

    let destroy = declare "destroy"
      [ "[destroy task sr vdi] removes [vdi] from [sr]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let stat = declare "stat"
      [ "[stat dbg sr vdi] returns information about VDI [vdi] in SR [sr]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning vdi_info_p storage_err)

    let introduce =
      let location_p = string_param "location" [ "Location to check for VDI" ] in
      let uuid_p     = string_param "uuid"     [ "Unique ID of SR" ]           in
      declare "introduce"
        [ "[introduce dbg sr uuid sm_config location] checks that a VDI exists "
        ; "and returns info about it" ]
        (dbg_p @-> sr_p @-> uuid_p @-> sm_conf_p @-> location_p @-> returning vdi_info_p storage_err)

    let persistent_p = Param.make ~name:"persistent" ~description:[ "Is storage object persistent?" ] Types.bool

    let set_persistent = declare "set_persistent"
      [ "[set_persistent dbg sr vdi persistent] sets [vdi]'s persistent flag to [persistent]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> persistent_p @-> returning unit_p storage_err)

    let epoch_begin = declare "epoch_begin"
      [ "[epoch_begin sr vdi persistent] declares that [vdi] is about to be added to a"
      ; " starting/rebooting VM. This is not called over suspend/resume or migrate."
      ; " If [persistent] is false, changes to the disk will be erased when the "
      ; "VM shuts down." ]
      (dbg_p @-> sr_p @-> vdi_p @-> persistent_p @-> returning unit_p storage_err)

    let attach =
      let read_write_p = Param.mk ~name:"read-write" ~description:[ "Is the VDI read-write?" ] Types.bool in
      declare "attach"
        [ "[attach task dp sr vdi read_write] returns the [params] for a given [vdi] in "
        ; "[sr] which can be written to if (but not necessarily only if) [read_write] "
        ; "is true" ]
        (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> read_write_p @-> returning attach_info storage_err)

    let activate = declare "activate"
      [ "[activate task dp sr vdi] signals the desire to immediately use [vdi]. "
      ; "This client must have called [attach] on the [vdi] first." ]
      (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let deactivate = declare "deactivate"
      [ "[deactivate task dp sr vdi] signals that this client has stopped reading (and writing) "
      ; " [vdi]." ]
      (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let detach = declare "detach"
      [ "[detach task dp sr vdi] signals that this client no-longer needs the [attach_info]"
      ; " to be valid." ]
      (dbg_p @-> dp_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let epoch_end = declare "epoch_end"
      [ "[epoch_end sr vdi] declares that [vdi] is about to be removed from a shutting "
      ; "down/rebooting VM. This is not called over suspend/resume or migrate." ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let get_url = declare "get_url"
      [ "[get_url task sr vdi] returns a URL suitable for accessing disk data directly." ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning string_p storage_err)

    let similar_content =
      declare "similar_content"
        [ "[similar_content task sr vdi] returns a list of VDIs which have similar content "
        ; "to [vdi]" ]
        (dbg_p @-> sr_p @-> vdi_p @-> returning vdi_info_list_p storage_err)

    let get_by_name = declare "get_by_name"
      [ "[get_by_name task sr name] returns the vdi within [sr] with [name]" ]
      (dbg_p @-> sr_p @-> name_p @-> returning vdi_info_p storage_err)

    let set_content_id =
      let content_id_p = Param.mk ~name:"content ID" ~description:[ "ID of content" ] content_id in
      declare "set_content_id"
        [ "[set_content_id task sr vdi content_id] tells the storage backend that a VDI "
        ; "has an updated [content_id]" ]
        (dbg_p @-> sr_p @-> vdi_p @-> content_id_p @-> returning unit_p storage_err)

    let compose =
      let vdi1_p = Param.mk ~name:"vdi1" ~description:[ "VDI to be updated" ]  vdi in
      let vdi1_p = Param.mk ~name:"vdi2" ~description:[ "VDI to update from" ] vdi in
      declare "compose"
        [ "[compose task sr vdi1 vdi2] layers the updates from [vdi2] onto [vdi1], "
        ; "modifying [vdi2]" ]
        (dbg_p @-> sr_p @-> vdi1_p @-> vdi2_p @-> returning unit_p storage_err)

    let key_p = Param.mk ~name:"key" ~description:[ "Key of sm_config" ] Types.string

    let add_to_sm_config =
      let value_p = string_param "value" [ "Value of sm config" ] in
      declare "add_to_sm_config"
        [ "[add_to_sm_config dbg sr vdi key value] associates [value] to the [key] "
        ; "in [vdi] sm-config" ]
        (dbg_p @-> sr_p @-> vdi_p @-> key_p @-> value_p @-> returning unit_p storage_err)

    let remove_from_sm_config = declare "remove_from_sm_config"
      [ "[remove_from_sm_config dbg sr vdi key] remove [key] from [vdi] sm-config" ]
      (dbg_p @-> sr_p @-> vdi_p @-> key_p @-> returning unit_p storage_err)

    let enable_cbt = declare "enable_cbt"
      [ "[enable_cbt dbg sr vdi] enables changed block tracking for [vdi]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let disable_cbt = declare "disable_cbt"
      [ "[disable_cbt dbg sr vdi] disables changed block tracking for [vdi]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let data_destroy = declare "data_destroy"
      [ "[data_destroy dbg sr vdi] deletes the data of the snapshot [vdi] without "
      ; "deleting its changed block tracking metadata" ]
      (dbg_p @-> sr_p @-> vdi_p @-> returning unit_p storage_err)

    let list_changed_blocks =
      let vdi_from_p = Param.mk ~name:"vdi1" ~description:[ "Base VDI without changes" ]       vdi in
      let vdi_to_p   = Param.mk ~name:"vdi2" ~description:[ "VDI to compare changes against" ] vdi in
      declare "list_changed_blocks"
        [ "[list_changed_blocks dbg sr vdi_from vdi_to] returns the blocks that have "
        ; "changed between [vdi_from] and [vdi_to] as a base64-encoded bitmap string" ]
        (dbg_p @-> sr_p @-> vdi_from_p @-> vdi_to_p @-> returning string_p storage_err)

  end

  let get_by_name =
    let module X = struct type sr_vdi_pair = (sr * vdi_info) [@@deriving rpcty] end in
    let pair_p = Param.mk ~description:[ "VDI with associated SR" ] X.sr_vdi_pair in
    declare "get_by_name"
      [ "[get_by_name task name] returns a vdi with [name] (which may be in any SR)" ]
      (dbg_p @-> name_p @-> returning pair_p storage_err)

  module DATA = struct

    let task_id_p = Param.mk ~name:"task ID" ~description:[ "ID of task" ]     Task.id
    let dest_sr_p = Param.mk ~name:"dest"    ~description:[ "Destination SR" ] sr
    let copy_into = declare "copy_into"
      [ "[copy_into task sr vdi url sr2] copies the data from [vdi] into a remote system [url]'s [sr2]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> url_p @-> dest_sr_p @-> dest_vdi_p @-> returning task_id_p storage_err)

    let copy = declare "copy"
      [ "[copy dbg sr vdi dp url dest_sr] Copies data from [vdi] in [sr] to [dest_sr] "
      ; "at [url] via [dp]" ]
      (dbg_p @-> sr_p @-> vdi_p @-> dp_p @-> url_p @-> dest_sr_p @-> returning task_id_p storage_err)


    module MIRROR = struct

      let mirror_t_p   = Param.mk                   ~description:[ "Mirror type" ]     Mirror.t
      let mirror_sim_p = Param.mk ~name:"similar"   ~description:[ "Similar mirrors" ] Mirror.similars
      let mirror_id_p  = Param.mk ~name:"id"        ~description:[ "ID of mirror" ]    Mirror.id

      let start = declare "start"
        [ "[start task sr vdi url sr2] creates a VDI in remote [url]'s [sr2] and writes "
        ; " data synchronously. It returns the id of the VDI." ]
        (dbg_p @-> sr_p @-> vdi_p @-> dp_p @-> url_p @-> dest_sr_p @-> returning task_id_p storage_err)

      [ "[stop task sr vdi] stops mirroring local [vdi]" ]
      let stop = declare "stop"
          [ "docstring" ]
          (dbg_p @-> mirror_id_p @-> returning unit_p storage_err)

      let stat = declare "stat"
          [ "docstring" ]
          (dbg_p @-> mirror_id_p @-> returning mirror_t_p storage_err)

      let receive_start =
        let mirr_res_p = Param.mk ~description:[ "Mirror result" ] Mirror.mirror_receive_result in
        declare "receive_start"
          [ "Called on the receiving end" ]
          (dbg_p @-> sr_p @-> vdi_info_p @-> mirror_id_p @-> mirror_sim_p @-> returning mirr_res_p storage_err)

      let receive_finalize = declare "receive_finalize"
          [ "docstring" ]
          (dbg_p @-> mirror_id_p @-> returning unit_p storage_err)

      let receive_cancel = declare "receive_cancel"
          [ "docstring" ]
          (dbg_p @-> mirror_id_p @-> returning unit_p storage_err)

      let list =
          let module M = struct
              type mirror_list = (Mirror.id * Mirror.t) list [@@deriving rpcty]
          end in
          let mirr_list_p = Param.mk ~description:[ "List of mirrors" ] M.mirror_list in
          declare "list"
          [ "docstring" ]
          (dbg_p @-> returning mirr_list_p storage_err)
    end
  end

  module Policy = struct
    let get_backend_vm =
      let vm_p = Param.mk ~name:"vm" ~description:
          [ "Virtual machine" ] Types.string in
      declare "get_backend_vm"
        [ "docstring" ]
        (dbg_p @-> vm_p @-> sr_p @-> vdi_p @-> returning string_p storage_err)
  end

  module TASK = struct
    let stat =
        let task_t_p = Param.mk ~description:[ "Task" ] Task.t in
        declare "stat"
        [ "docstring" ]
        (dbg_p @-> task_id_p @-> returning task_t_p storage_err)

    let cancel = declare "cancel"
        [ "docstring" ]
        (dbg_p @-> task_id_p @-> returning unit_p storage_err)

    let destroy = declare "destroy"
        [ "docstring" ]
        (dbg_p @-> task_id_p @-> returning unit_p storage_err)

    let list =
        let module T = struct
          type lst = Task.t list [@@deriving rpcty]
        end in
        let task_lst_p = Param.mk ~description:[ "Task list" ] T.lst in
        declare "list"
        [ "docstring" ]
        (dbg_p @-> returning task_lst_p storage_err)
  end

  module UPDATES = struct
    let get =
        let module U = struct
          type int_opt = int option               [@@deriving rpcty]
          type lst_str = Dynamic.id list * string [@@deriving rpcty]
        end in
        let from_p = Param.mk ~name:"from" ~description:[ "source of updates" ] Types.string in
        let timeout_p = Param.mk ~name:"timeout" ~description:[ "[optional] how long to wait for this request" ] U.int_opt in
        let lst_p = Param.mk ~description:[ "List of updates" ] U.lst_str in
        declare "get"
        [ "docstring" ]
        (dbg_p @-> from_p @-> timeout_p @-> returning lst_p storage_err)
  end
end
