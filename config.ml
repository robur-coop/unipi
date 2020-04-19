open Mirage

let hook =
  let doc = Key.Arg.info ~doc:"GitHub push hook." ["hook"] in
  Key.(create "hook" Arg.(opt string "hook" doc))

let remote =
  let doc = Key.Arg.info
      ~doc:"Remote repository url, use suffix #foo to specify a branch 'foo': \
            https://github.com/hannesm/unipi.git#gh-pages"
      ["remote"]
  in
  Key.(create "remote"
         Arg.(opt string "https://github.com/hannesm/unipi#gh-pages" doc))

let port =
  let doc = Key.Arg.info ~doc:"HTTP listen port." ["port"] in
  Key.(create "port" Arg.(opt int 80 doc))

let tls_port =
  let doc = Key.Arg.info ~doc:"Enable TLS on given port." ["tls"] in
  Key.(create "tls" Arg.(opt (some int) None doc))

let packages = [
  package ~min:"2.0.0" "irmin";
  package ~min:"2.0.0" "irmin-mirage";
  package ~min:"2.0.0" "irmin-mirage-git";
  package "cohttp-mirage";
  package "tls-mirage";
  package "magic-mime";
  package "logs";
]

let stack = generic_stackv4 default_network

let () =
  let keys = Key.([
      abstract hook; abstract remote;
      abstract port; abstract tls_port;
    ])
  in
  register "unipi" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (stackv4 @-> resolver @-> conduit @-> pclock @-> job)
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_posix_clock
  ]
