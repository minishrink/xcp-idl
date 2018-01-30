(* Licensing debugging CLI *)

module V6_Cli = Cli(struct
    include V6_interface

    let cli_docstring =
      "A CLI for the V6d API. This allows scripting of the licensing daemon for testing and debugging. This tool is not intended to be used as an end user tool"

    let cli_title = "licensing_cli"
  end) (V6_client)

let _ = V6_Cli.cli ()
