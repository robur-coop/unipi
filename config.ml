(* mirage >= 4.9.0 & < 4.10.0 *)

open Mirage

let packages = [
  package ~min:"3.7.0" "git-paf";
  package ~min:"3.7.0" "git";
  package ~min:"0.0.5" "git-kv";
  package "tls-mirage";
  package ~min:"1.3.0" "magic-mime";
  package "logs";
  package "awa";
  package "awa-mirage";
  package ~min:"0.3.0" "letsencrypt";
  package ~min:"0.5.0" "paf" ~sublibs:[ "mirage" ];
  package ~min:"0.0.6" "http-mirage-client";
  package "letsencrypt-mirage";
  package "ohex";
]

let unipi =
  main "Unikernel.Main" ~packages
    (git_client @-> stackv4v6 @-> alpn_client @-> job)

let enable_monitoring =
  let doc = Key.Arg.info
      ~doc:"Enable monitoring (syslog, metrics to influx, log level, statmemprof tracing)"
      [ "enable-monitoring" ]
  in
  Key.(create "enable-monitoring" Arg.(flag doc))

let stack = generic_stackv4v6 default_network

let management_stack =
  if_impl
    (Key.value enable_monitoring)
    (generic_stackv4v6 ~group:"management" (netif ~group:"management" "management"))
    stack

let name =
  runtime_arg ~pos:__POS__
    {|let doc = Cmdliner.Arg.info ~doc:"Name of the unikernel"
        ~docs:Mirage_runtime.s_log [ "name" ]
      in
      Cmdliner.Arg.(value & opt string "robur.coop" doc)|}

let monitoring =
  let monitor = Runtime_arg.(v (monitor None)) in
  let connect _ modname = function
    | [ stack ; name ; monitor ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no monitor specified, not outputting statistics\")\
         | Some ip -> %s.create ip ~hostname:%s %s)"
        monitor modname name stack
    | _ -> assert false
  in
  impl
    ~packages:[ package ~min:"0.0.6" "mirage-monitoring" ]
    ~runtime_args:[ name ; monitor ]
    ~connect "Mirage_monitoring.Make"
    (stackv4v6 @-> job)

let syslog =
  let syslog = Runtime_arg.(v (syslog None)) in
  let connect _ modname = function
    | [ stack ; name ; syslog ] ->
      code ~pos:__POS__
        "Lwt.return (match %s with\
         | None -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\")\
         | Some ip -> Logs.set_reporter (%s.create %s ip ~hostname:%s ()))"
        syslog modname stack name
    | _ -> assert false
  in
  impl
    ~packages:[ package ~sublibs:["mirage"] ~min:"0.5.0" "logs-syslog" ]
    ~runtime_args:[ name ; syslog ]
    ~connect "Logs_syslog_mirage.Udp"
    (stackv4v6 @-> job)

let optional_monitoring stack =
  if_impl (Key.value enable_monitoring)
    (monitoring $ stack)
    noop

let optional_syslog stack =
  if_impl (Key.value enable_monitoring)
    (syslog $ stack)
    noop

let he = generic_happy_eyeballs stack
let dns = generic_dns_client stack he

let alpn_client =
  let dns = mimic_happy_eyeballs stack he dns in
  paf_client (tcpv4v6_of_stackv4v6 stack) dns

let git_client =
  let git = mimic_happy_eyeballs stack he dns in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh tcp git) (git_http tcp git))

let () =
  register "unipi" [
    optional_syslog management_stack ;
    optional_monitoring management_stack ;
    unipi $ git_client $ stack $ alpn_client
  ]
