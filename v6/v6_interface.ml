(*
 * Copyright (C) Citrix Systems Inc.
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

open Rpc
open Idl

let service_name = "v6d"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)
let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)
let uri () = "file:" ^ !default_path

(* --- global types used in most API calls --- *)

type debug_info = string
(** Uninterpreted/sanitised string associated with operation *)
[@@deriving rpcty]

type edition =
{
  name : string ;
  (** Name of edition, this will be passed to apply_edition *)
  official_title : string ;
  (** Marketing title used to advertise edition *)
  code : string ;
  (** Abbreviated form of name, used to show up in logs and on command line *)
  order : int ;
  (** Number indicating ordering among other editions
      with low numbers corresponding to editions with fewer features, and vice versa *)
}
[@@deriving rpcty]

type edition_list = edition list
[@@deriving rpcty]

type edition_info = {
  edition: string;
	(** Name of edition *)
  xapi_params: (string * string) list;
	(** List of parameters used by Xapi *)
  additional_params: (string * string) list;
	(** Addition parameters supplied *)
  experimental_features: (string * bool) list;
	(** List of experimental features and whether they're available in this edition *)
}
[@@deriving rpcty]

(* --- errors wrapped in generic errors type --- *)

type errors =
  | Invalid_edition of string
  (** Thrown by apply_edition on receiving unfamiliar edition. Note: get_editions returns list of all valid editions *)
  | V6d_failure
  (** Licensing daemon failed *)
  | License_expired
  (** Thrown by license_check when expiry date matches or precedes current date *)
  | License_processing_error
  (** License could not be processed *)
  | Missing_connection_details
  (** Thrown if connection port or address parameter not supplied to check_license *)
[@@deriving rpcty]

(* --- API interface --- *)

module API(R : RPC) = struct
  open R

  (* description of functions *)
  let description = Interface.{
      name = "Licensing";
      namespace = None;
      description = [
        "This interface is used by Xapi and V6d to manage ";
        "XenServer edition licensing of hosts.";
        ];
      version=(1,0,0);        }

  (* define implementation *)
  let implementation = implement description

  (* define global parameters for API calls *)
  let debug_info_p = Param.mk ~description:[
    "An uninterpreted string to associate with the operation."
    ] Types.string 

  (* dbg_str -> requested edition -> current params -> edition_info *)
  let apply_edition =
    declare
    "apply_edition"
    ["Checks license info and ensures provided features are compatible."]
    ( debug_info_p @-> edition_p : string @-> current_params : (string * string) list @-> edition_info )

  (* dbg_str @-> list of editions *)
  let get_editions =
    declare
    "get_editions"
    ["Gets list of accepted editions."]
    ( debug_info @-> edition_list )

  (* dbg_str @-> result *)
  let get_version =
    let result_p = Param.mk ~description:["String of version."] Types.string in
    declare
    "get_version"
    ["Returns version"]
    ( debug_info @-> returning result_p )

end
