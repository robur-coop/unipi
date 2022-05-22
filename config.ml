open Mirage

(* boilerplate from https://github.com/mirage/ocaml-git.git
   file unikernel/empty-commit/config.ml
   commit #45d90b8792ab8f3866751f462619c7dd7860e5d5 *)
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

(* TODO(dinosaure): [timeout] and [timer interval]. *)
let mimic_happy_eyeballs =
  let packages = [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = random @-> time @-> mclock @-> pclock @-> stackv4v6 @-> mimic
       method module_name = "Git_mirage_happy_eyeballs.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_happy_eyeballs"
       method! connect _ modname = function
         | [ _random; _time; _mclock; _pclock; stackv4v6; ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end

let mimic_tcp =
  let packages = [ package "git-mirage" ~sublibs:[ "tcp" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = tcpv4v6 @-> mimic @-> mimic
       method module_name = "Git_mirage_tcp.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_tcp"
       method! connect _ modname = function
         | [ _tcpv4v6; ctx ] ->
           Fmt.str {ocaml|%s.connect %s|ocaml}
             modname ctx
         | _ -> assert false
     end

let mimic_ssh ?authenticator key =
  let packages = [ package "git-mirage" ~sublibs:[ "ssh" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = mclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match authenticator with
         | Some authenticator -> [ Key.abstract key; Key.abstract authenticator ]
         | None -> [ Key.abstract key ]
       method module_name = "Git_mirage_ssh.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_ssh"
       method! connect _ modname = function
         | [ _mclock; _tcpv4v6; ctx ] ->
           ( match authenticator with
           | None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ~key:%a|ocaml}
               modname ctx modname Key.serialize_call (Key.abstract key)
           | Some authenticator ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optionnal_key ?authenticator:%a ~key:%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract authenticator)
               Key.serialize_call (Key.abstract key) )
         | _ -> assert false
     end

let mimic_http ?tls_key_fingerprint ?tls_cert_fingerprint headers =
  let packages = [ package "git-mirage" ~sublibs:[ "http" ] ] in
  impl @@ object
       inherit base_configurable
       method ty = time @-> pclock @-> tcpv4v6 @-> mimic @-> mimic
       method! keys = match tls_key_fingerprint, tls_cert_fingerprint with
         | Some tls_key_fingerprint, None ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint ] @ keys
         | None, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_cert_fingerprint ] @ keys
         | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
           let keys = match headers with Some headers -> [ Key.abstract headers ] | None -> [] in
           [ Key.abstract tls_key_fingerprint; Key.abstract tls_cert_fingerprint ] @ keys
         | None, None -> ( match headers with Some headers -> [ Key.abstract headers ] | None -> [] )
       method module_name = "Git_mirage_http.Make"
       method! packages = Key.pure packages
       method name = "git_mirage_http"
       method! connect _ modname = function
         | [ _time; _pclock; _tcpv4v6; ctx; ] ->
           let serialize_headers ppf = function
             | None -> ()
             | Some headers -> Fmt.pf ppf "?headers:%a" Key.serialize_call (Key.abstract headers) in
           ( match tls_key_fingerprint, tls_cert_fingerprint with
           | Some tls_key_fingerprint, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_key_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers
           | None, None ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers%a|ocaml}
               modname ctx modname
               Fmt.((const string " ") ++ serialize_headers) headers
           | Some tls_key_fingerprint, Some tls_cert_fingerprint ->
             Fmt.str {ocaml|%s.connect %s >>= %s.with_optional_tls_config_and_headers
                              ?tls_key_fingerprint:%a ?tls_cert_fingerprint:%a%a|ocaml}
               modname ctx modname
               Key.serialize_call (Key.abstract tls_key_fingerprint)
               Key.serialize_call (Key.abstract tls_cert_fingerprint)
               Fmt.((const string " ") ++ serialize_headers) headers )
         | _ -> assert false
     end

let tcpv4v6_of_stackv4v6 =
  impl @@ object
       inherit base_configurable
       method ty = stackv4v6 @-> tcpv4v6
       method module_name = "Git_mirage_happy_eyeballs.TCPV4V6"
       method! packages = Key.pure [ package "git-mirage" ~sublibs:[ "happy-eyeballs" ] ]
       method name = "tcpv4v6"
       method! connect _ modname = function
         | [ stackv4v6 ] -> Fmt.str {ocaml|%s.connect %s|ocaml} modname stackv4v6
         | _ -> assert false
     end
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

let https_port =
  let doc = Key.Arg.info ~doc:"HTTPS listen port." ["https-port"] in
  Key.(create "https-port" Arg.(opt int 443 doc))

let tls =
  let doc = Key.Arg.info ~doc:"Enable TLS." ["tls"] in
  Key.(create "tls" Arg.(opt bool false doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." ["ssh-authenticator"] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let tls_key_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS key of the git remote." [ "tls-key-fingerprint" ] in
  Key.(create "tls_key_fingerprint" Arg.(opt (some string) None doc))

let tls_cert_fingerprint =
  let doc = Key.Arg.info ~doc:"The fingerprint of the TLS certificate of the git remote." [ "tls-cert-fingerprint" ] in
  Key.(create "tls_cert_fingerprint" Arg.(opt (some string) None doc))

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
  package ~min:"3.7.0" "git-paf";
  package ~min:"3.7.0" "git";
  package ~min:"3.7.0" ~max:"3.8.0" "git-mirage";
  package ~min:"2.10.0" ~max:"3.0.0" "irmin-mirage-git";
  package "tls-mirage";
  package "magic-mime";
  package "logs";
  package "awa";
  package "awa-mirage";
  package ~min:"3.7.0" "git-mirage";
  package ~min:"0.3.0" "letsencrypt";
  package "paf" ~min:"0.0.8-1" ~max:"0.0.9" ~sublibs:[ "mirage" ];
  package "paf-le" ~min:"0.0.8-1" ~max:"0.0.9";
]

let stack = generic_stackv4v6 default_network

let mimic_impl random stackv4v6 mclock pclock time =
  let tcpv4v6 = tcpv4v6_of_stackv4v6 $ stackv4v6 in
  let mhappy_eyeballs = mimic_happy_eyeballs $ random $ time $ mclock $ pclock $ stackv4v6 in
  let mtcp  = mimic_tcp
    $ tcpv4v6 $ mhappy_eyeballs in
  let mssh  = mimic_ssh ~authenticator:ssh_authenticator ssh_key
    $ mclock $ tcpv4v6 $ mhappy_eyeballs in
  let mhttp = mimic_http ~tls_key_fingerprint ~tls_cert_fingerprint None
    $ time $ pclock $ tcpv4v6 $ mhappy_eyeballs in
  merge mhttp (merge mtcp mssh)

let mimic_impl =
  mimic_impl default_random stack
    default_monotonic_clock default_posix_clock default_time

let () =
  let keys = Key.([
      abstract hook; abstract remote;
      abstract port; abstract https_port; abstract tls;
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
