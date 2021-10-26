open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git
   file unikernel/empty-commit/config.ml
   commit #ecdfc6dc13834f5f1a8e378718512eda6e67c982 *)
type mimic = Mimic

let mimic = typ Mimic

let mimic_count =
  let v = ref (-1) in
  fun () -> incr v ; !v

let mimic_conf () =
  let packages = [ package "mimic" ] in
  impl @@ object
       inherit base_configurable
       method ty = mimic @-> mimic @-> mimic
       method module_name = "Mimic.Merge"
       method! packages = Key.pure packages
       method name = Fmt.str "merge_ctx%02d" (mimic_count ())
       method! connect _ _modname =
         function
         | [ a; b ] -> Fmt.str "Lwt.return (Mimic.merge %s %s)" a b
         | [ x ] -> Fmt.str "%s.ctx" x
         | _ -> Fmt.str "Lwt.return Mimic.empty"
     end

let merge ctx0 ctx1 = mimic_conf () $ ctx0 $ ctx1

let mimic_tcp_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "tcp_ctx"
       method! connect _ modname = function
         | [ stack ] ->
           Fmt.str {ocaml|Lwt.return (%s.with_stack %s %s.ctx)|ocaml}
             modname stack modname
         | _ -> assert false
     end

let mimic_tcp_impl stackv4v6 = mimic_tcp_conf $ stackv4v6

let mimic_ssh_conf ~kind ~seed ~auth =
  let seed = Key.abstract seed in
  let auth = Key.abstract auth in
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> mimic @-> mclock @-> mimic
       method! keys = [ seed; auth; ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = match kind with
         | `Rsa -> "ssh_rsa_ctx"
         | `Ed25519 -> "ssh_ed25519_ctx"
       method! connect _ modname =
         function
         | [ _; tcp_ctx; _ ] ->
             let with_key =
               match kind with
               | `Rsa -> "with_rsa_key"
               | `Ed25519 -> "with_ed25519_key"
             in
             Fmt.str
               {ocaml|let ssh_ctx00 = Mimic.merge %s %s.ctx in
                      let ssh_ctx01 = Option.fold ~none:ssh_ctx00 ~some:(fun v -> %s.%s v ssh_ctx00) %a in
                      let ssh_ctx02 = Option.fold ~none:ssh_ctx01 ~some:(fun v -> %s.with_authenticator v ssh_ctx01) %a in
                      Lwt.return ssh_ctx02|ocaml}
               tcp_ctx modname
               modname with_key Key.serialize_call seed
               modname Key.serialize_call auth
         | _ -> assert false
     end

let mimic_ssh_impl ~kind ~seed ~auth stackv4v6 mimic_git mclock =
  mimic_ssh_conf ~kind ~seed ~auth
  $ stackv4v6
  $ mimic_git
  $ mclock

(* TODO(dinosaure): user-defined nameserver and port. *)

let mimic_dns_conf =
  let packages = [ package "git-mirage" ~sublibs:[ "dns" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> mclock @-> pclock @-> time @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_dns.Make"
       method! packages = Key.pure packages
       method name = "dns_ctx"
       method! connect _ modname =
         function
         | [ _; _; _; _; stack; tcp_ctx ] ->
             Fmt.str
               {ocaml|let dns_ctx00 = Mimic.merge %s %s.ctx in
                      let dns_ctx01 = %s.with_dns %s dns_ctx00 in
                      Lwt.return dns_ctx01|ocaml}
               tcp_ctx modname
               modname stack
         | _ -> assert false
     end

let mimic_dns_impl random mclock pclock time stackv4v6 mimic_tcp =
  mimic_dns_conf $ random $ mclock $ pclock $ time $ stackv4v6 $ mimic_tcp

let mimic_paf_conf () =
  let packages = [ package "git-paf" ] in
  impl @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> stackv4v6 @-> mimic @-> mimic
       method module_name = "Git_paf.Make"
       method! packages = Key.pure packages
       method name = "paf_ctx"
       method! connect _ modname = function
         | [ _; _; _; tcp_ctx; ] ->
             Fmt.str
               {ocaml|let paf_ctx00 = Mimic.merge %s %s.ctx in
                      Lwt.return paf_ctx00|ocaml}
               tcp_ctx modname
         | _ -> assert false
     end

let mimic_paf_impl time pclock stackv4v6 mimic_tcp =
  mimic_paf_conf ()
  $ time
  $ pclock
  $ stackv4v6
  $ mimic_tcp
(* --- end of copied code --- *)

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
  package ~min:"3.5.0" "git-paf";
  package ~min:"3.5.0" "git";
  package ~min:"3.5.0" "git-mirage";
  package ~min:"2.8.0" ~max:"3.0.0" "irmin-mirage-git";
  package "tls-mirage";
  package "magic-mime";
  package "logs";
  package "awa";
  package "awa-mirage";
  package ~min:"3.6.0" ~max:"3.7.0" "git-mirage";
  package ~min:"0.3.0" "letsencrypt";
  package "paf" ~min:"0.0.6" ~sublibs:[ "mirage" ];
  package "paf-le";
]

let stack = generic_stackv4v6 default_network

let mimic_impl ~kind ~seed ~authenticator stackv4v6 random mclock pclock time =
  let mtcp = mimic_tcp_impl stackv4v6 in
  let mdns = mimic_dns_impl random mclock pclock time stackv4v6 mtcp in
  let mssh = mimic_ssh_impl ~kind ~seed ~auth:authenticator stackv4v6 mtcp mclock in
  let mpaf = mimic_paf_impl time pclock stackv4v6 mtcp in
  merge mpaf (merge mssh mdns)

let mimic_impl =
  mimic_impl ~kind:`Rsa ~seed:ssh_seed ~authenticator:ssh_authenticator stack
    default_random default_monotonic_clock default_posix_clock default_time

let () =
  let keys = Key.([
      abstract hook; abstract remote;
      abstract port; abstract tls;
      abstract ssh_seed; abstract ssh_authenticator;
      abstract hostname; abstract production;
      abstract cert_seed; abstract cert_key_type; abstract cert_bits;
      abstract account_seed; abstract account_key_type; abstract account_bits;
      abstract email;
    ])
  in
  register "unipi" [
    foreign
      ~keys
      ~packages
      "Unikernel.Main"
      (mimic @-> random @-> mclock @-> pclock @-> time @-> stackv4v6 @-> job)
    $ mimic_impl
    $ default_random
    $ default_monotonic_clock
    $ default_posix_clock
    $ default_time
    $ stack
  ]
