open Mirage

let hook =
  let doc = Key.Arg.info ~doc:"Webhook (no / allowed)." ["hook"] in
  Key.(create "hook" Arg.(opt string "hook" doc))

let remote =
  let doc = Key.Arg.info
      ~doc:"Remote repository url, use suffix #foo to specify a branch 'foo': \
            https://github.com/hannesm/unipi.git#gh-pages"
      ["remote"]
  in
  Key.(create "remote" Arg.(required string doc))

let port =
  let doc = Key.Arg.info ~doc:"HTTP listen port." ["port"] in
  Key.(create "port" Arg.(opt int 80 doc))

let tls =
  let doc = Key.Arg.info ~doc:"Enable TLS." ["tls"] in
  Key.(create "tls" Arg.(opt bool false doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed for ssh private key." ["ssh-seed"] in
  Key.(create "ssh_seed" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." ["ssh-authenticator"] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Host name." ["hostname"] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let production =
  let doc = Key.Arg.info ~doc:"Let's encrypt production environment." ["production"] in
  Key.(create "production" Arg.(opt bool false doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." ["cert-seed"] in
  Key.(create "cert_seed" Arg.(opt (some string) None doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." ["account-seed"] in
  Key.(create "account_seed" Arg.(opt (some string) None doc))

let email =
  let doc = Key.Arg.info ~doc:"Let's encrypt E-Mail." ["email"] in
  Key.(create "email" Arg.(opt (some string) None doc))

let awa_pin = "git+https://github.com/hannesm/awa-ssh.git#future"
and git_pin = "git+https://github.com/hannesm/ocaml-git.git#awa-future"
and conduit_pin = "git+https://github.com/hannesm/ocaml-conduit.git#awa-future"

let packages = [
  package ~min:"2.0.0" "irmin";
  package ~min:"2.0.0" "irmin-mirage";
  package ~min:"2.0.0" "irmin-mirage-git";
  package "cohttp-mirage";
  package "tls-mirage";
  package "magic-mime";
  package "logs";
  package ~pin:awa_pin "awa";
  package ~pin:awa_pin "awa-mirage";
  package ~pin:conduit_pin "conduit";
  package ~pin:conduit_pin "conduit-lwt";
  (* let's encrypt depends on cohttp-lwt-unix which depends on conduit-lwt-unix *)
  package ~build:true ~pin:conduit_pin "conduit-lwt-unix";
  package ~pin:conduit_pin "conduit-mirage";
  package ~pin:git_pin "git";
  package ~pin:git_pin "git-http";
  package ~pin:git_pin "git-mirage";
  package "letsencrypt";
]

let stack = generic_stackv4 default_network

let () =
  let keys = Key.([
      abstract hook; abstract remote;
      abstract port; abstract tls;
      abstract ssh_seed; abstract ssh_authenticator;
      abstract hostname; abstract production; abstract cert_seed;
      abstract account_seed; abstract email;
    ])
  in
  register "unipi" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (stackv4 @-> resolver @-> conduit @-> pclock @-> mclock @-> time @-> job)
    $ stack
    $ resolver_dns stack
    $ conduit_direct ~tls:true stack
    $ default_posix_clock
    $ default_monotonic_clock
    $ default_time
  ]
