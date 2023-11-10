open Mirage

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
  foreign "Unikernel.Main"
    ~packages
    (git_client @-> pclock @-> time @-> stackv4v6 @-> alpn_client @-> job)

let enable_monitoring =
  let doc = Cmdliner.Arg.info
      ~doc:"Enable monitoring (only available for solo5 targets)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let stack = generic_stackv4v6 default_network

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    stack

let (name : string Runtime_key.key) = Runtime_key.create "Unikernel.K.name"

let monitoring =
  let monitor = Runtime_key.create "Unikernel.K.monitor" in
  let connect _ modname = function
    | [ _ ; _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
               | Some ip -> %s.create ip ~hostname:%a %s)"
        Runtime_key.call monitor modname
        Runtime_key.call name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package "mirage-monitoring" ]
    ~runtime_keys:[ Runtime_key.v name ; Runtime_key.v monitor ]
    ~connect "Mirage_monitoring.Make"
    (time @-> pclock @-> stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_key.create "Unikernel.K.syslog" in
  let connect _ modname = function
    | [ _ ; stack ] ->
      Fmt.str "Lwt.return (match %a with\
               | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
               | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:%a ()))"
        Runtime_key.call syslog modname stack
        Runtime_key.call name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.4.0" "logs-syslog" ]
    ~runtime_keys:[ Runtime_key.v name ; Runtime_key.v syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (pclock @-> stackv4v6 @-> job)

type i0 = I0
let i0 = Functoria.Type.v I0
let no0 = Functoria.impl "Int" job

type n1 = N1
let n1 = Functoria.Type.v N1
let noop1 = Functoria.impl "Set.Make" (job @-> job)

let optional_monitoring time pclock stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ time $ pclock $ stack)
    (noop1 $ no0)

let optional_syslog pclock stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ pclock $ stack)
    (noop1 $ no0)

let dns = generic_dns_client stack

let alpn_client =
  let dns =
    mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns)
  in
  paf_client (tcpv4v6_of_stackv4v6 stack) dns

let ssh_key = Runtime_key.create "Unikernel.K.ssh_key"
let ssh_password = Runtime_key.create "Unikernel.K.ssh_password"
let ssh_authenticator = Runtime_key.create "Unikernel.K.ssh_authenticator"
let tls_authenticator = Runtime_key.create "Unikernel.K.tls_authenticator"

let git_client =
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git)
       (git_http ~authenticator:tls_authenticator tcp git))

let () =
  register "unipi" [
    optional_syslog default_posix_clock management_stack ;
    optional_monitoring default_time default_posix_clock management_stack ;
    unipi $ git_client $ default_posix_clock $ default_time $ stack $ alpn_client
  ]
