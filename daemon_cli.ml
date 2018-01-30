(* Daemon CLI *)

module Cli (Daemon_interface : sig
    val cli_docstring : string list
    val cli_title : string
    module RPC_API : functor (R : RPC) -> sig end
  end)
    (Daemon_client : sig
       val rpc : 'a -> 'b
     end) = struct
  module Cmds = Daemon_interface.RPC_API(Cmdlinergen.Gen ())

  let version_str description =
    let maj,min,mic = description.Idl.Interface.version in
    Printf.sprintf "%d.%d.%d" maj min mic

  let default_cmd =
    let doc = Daemon_interface.cli_docstring in
    Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
    Cmdliner.Term.info Daemon_interface.cli_title ~version:(version_str Cmds.description) ~doc

  let cli () =
    let rpc = Daemon_client.rpc in
    Cmdliner.Term.eval_choice default_cmd (List.map (fun t -> t rpc) (Cmds.implementation ()))

end

let _ = cli ()
