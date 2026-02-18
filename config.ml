(* mirage >= 4.10.0 & < 4.11.0 *)

open Mirage

let packages = [
  package ~min:"0.2.0" "git-kv";
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

  package "charrua" ~pin:"git+https://github.com/mirage/charrua.git#main";
  package "charrua-client" ~sublibs:["mirage"] ~pin:"git+https://github.com/mirage/charrua.git#main";
  package "dnsvizor-csr" ~pin:"git+https://github.com/robur-coop/dnsvizor.git#vendor-identifying";
  package "dns-certify";
  package "dns-mirage";

  package "awa";
  package "awa-mirage";
  package "dns-client-mirage";
  package "duration";
  package "ethernet";
  package "git-kv";
  package "git-net";
  package "happy-eyeballs-mirage";
  package "mimic";
  package "mimic-happy-eyeballs";
  package "mirage-ptime";
  package "mirage-sleep";
  package "ohex";
  package "utcp" ~sublibs:[ "mirage" ];
  package "tcpip" ~sublibs:[ "ipv6"; "icmpv4"; "stack-direct"; "udp" ];

  package "mirage-runtime";
  package "mirage-runtime" ~sublibs:[ "network" ];
]

let unipi =
  main "Unikernel'.Main" ~packages
    (network @-> job)

let () =
  register "unipi" [
    unipi $ default_network
  ]
