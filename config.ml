open Mirage

let default_mime_type =
  let doc = Key.Arg.info ~doc:"Default mime-type to serve." ["default-mime-type"] in
  Key.(create "default-mime-type" Arg.(opt string "application/octet-stream" doc))

let mime_type =
  let doc = Key.Arg.info ~doc:"Overwrite mime-type for a path." ["mime-type"] in
  Key.(create "mime-type" Arg.(opt_all (pair ~sep:':' string string) doc))

let hook =
  let doc = Key.Arg.info ~doc:"Webhook for pulling the repository." ["hook"] in
  Key.(create "hook" Arg.(opt string "/hook" doc))

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

let https_port =
  let doc = Key.Arg.info ~doc:"HTTPS listen port." ["https-port"] in
  Key.(create "https-port" Arg.(opt int 443 doc))

let tls =
  let doc = Key.Arg.info ~doc:"Enable TLS." ["tls"] in
  Key.(create "tls" Arg.(opt bool false doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_password =
  let doc = Key.Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
  Key.(create "ssh-password" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." ["ssh-authenticator"] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let tls_authenticator =
  (* this will not look the same in the help printout *)
  let doc = "TLS host authenticator. See git_http in lib/mirage/mirage.mli for a description of the format."
  in
  let doc = Key.Arg.info ~doc ["tls-authenticator"] in
  Key.(create "tls-authenticator" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Host name (used for let's encrypt and redirects)." ["hostname"] in
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
  package ~min:"3.7.0" "git-paf";
  package ~min:"3.7.0" "git";
  package ~min:"0.0.2" "git-kv";
  package "tls-mirage";
  package ~min:"1.3.0" "magic-mime";
  package "logs";
  package "awa";
  package "awa-mirage";
  package ~min:"0.3.0" "letsencrypt";
  package ~min:"0.5.0" "paf" ~sublibs:[ "mirage" ];
  package ~min:"0.0.3" "http-mirage-client";
  package "letsencrypt-mirage";
]

let unipi =
  let keys = [
    Key.v default_mime_type; Key.v mime_type; Key.v hook; Key.v remote;
    Key.v port; Key.v https_port; Key.v tls;
    Key.v hostname; Key.v production;
    Key.v cert_seed; Key.v cert_key_type; Key.v cert_bits;
    Key.v account_seed; Key.v account_key_type; Key.v account_bits;
    Key.v email;
  ] in
  foreign "Unikernel.Main"
    ~packages ~keys
    (git_client @-> pclock @-> time @-> stackv4v6 @-> alpn_client @-> job)

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (only available for solo5 targets)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag ~stage:`Configure doc))

let stack = generic_stackv4v6 default_network

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    stack

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" [ "name" ] in
  Key.(v (create "name" Arg.(opt string "robur.coop" doc)))

let monitoring =
  let monitor =
    let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
    Key.(v (create "monitor" Arg.(opt (some ip_address) None doc)))
  in
  let connect _ modname = function
    | [ _ ; _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
               | Some ip -> %s.create ip ~hostname:%a %s)"
        Key.serialize_call monitor modname
        Key.serialize_call name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~keys:[ name ; monitor ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let syslog =
    let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
    Key.(v (create "syslog" Arg.(opt (some ip_address) None doc)))
  in
  let connect _ modname = function
    | [ console ; _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
               | Some ip -> Logs.set_reporter (%s.create %s %s ip ~hostname:%a ()))"
        Key.serialize_call syslog modname console stack
        Key.serialize_call name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.3.0" "logs-syslog" ]
    ~keys:[ name ; syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (console @-> pclock @-> stackv4v6 @-> job)

let optional_monitoring time pclock stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ time $ pclock $ stack)
    noop

let optional_syslog console pclock stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ console $ pclock $ stack)
    noop

let dns = generic_dns_client stack

let alpn_client =
  let dns =
    mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns)
  in
  paf_client (tcpv4v6_of_stackv4v6 stack) dns

let git_client =
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git)
       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  register "unipi" [
    optional_syslog default_console default_posix_clock management_stack ;
    optional_monitoring default_time default_posix_clock management_stack ;
    unipi $ git_client $ default_posix_clock $ default_time $ stack $ alpn_client
  ]
