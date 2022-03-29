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

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." ["ssh-authenticator"] in
  Key.(create "ssh-authenticator" Arg.(opt (some string) None doc))

let tls_authenticator =
  let doc = Key.Arg.info ~doc:"TLS host authenticator." [ "tls-authenticator" ] in
  Key.(create "tls-authenticator" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Host name." ["hostname"] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let production =
  let doc = Key.Arg.info ~doc:"Let's encrypt production environment." ["production"] in
  Key.(create "production" Arg.(opt bool false doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." ["cert-seed"] in
  Key.(create "cert-seed" Arg.(opt (some string) None doc))

let cert_key_type =
  let doc = Key.Arg.info ~doc:"certificate key type" ["cert-key-type"] in
  Key.(create "cert-key-type" Arg.(opt string "RSA" doc))

let cert_bits =
  let doc = Key.Arg.info ~doc:"certificate public key bits" ["cert-bits"] in
  Key.(create "cert-bits" Arg.(opt int 4096 doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." ["account-seed"] in
  Key.(create "account-seed" Arg.(opt (some string) None doc))

let account_key_type =
  let doc = Key.Arg.info ~doc:"account key type" ["account-key-type"] in
  Key.(create "account-key-type" Arg.(opt string "RSA" doc))

let account_bits =
  let doc = Key.Arg.info ~doc:"account public key bits" ["account-bits"] in
  Key.(create "account-bits" Arg.(opt int 4096 doc))

let email =
  let doc = Key.Arg.info ~doc:"Let's encrypt E-Mail." ["email"] in
  Key.(create "email" Arg.(opt (some string) None doc))

let packages = [
  package ~min:"2.10.0" ~max:"3.0.0" "irmin-mirage-git";
  package "tls-mirage";
  package "magic-mime";
  package "logs";
  package "awa";
  package "awa-mirage";
  package ~min:"0.3.0" "letsencrypt";
  package "paf" ~min:"0.0.8" ~sublibs:[ "mirage" ];
  package "paf-le" ~min:"0.0.8";
]

let stack = generic_stackv4v6 default_network

let git =
  let dns_client = generic_dns_client stack in
  let git = git_happy_eyeballs stack dns_client (generic_happy_eyeballs stack dns_client) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~authenticator:ssh_authenticator tcp git)
                       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  let keys = Key.([
      v hook; v remote;
      v port; v tls;
      v hostname; v production;
      v cert_seed; v cert_key_type; v cert_bits;
      v account_seed; v account_key_type; v account_bits;
      v email;
    ])
  in
  register "unipi" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (git_client @-> random @-> mclock @-> pclock @-> time @-> stackv4v6 @-> job)
    $ git
    $ default_random
    $ default_monotonic_clock
    $ default_posix_clock
    $ default_time
    $ stack
  ]
