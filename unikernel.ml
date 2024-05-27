open Lwt.Infix

module K = struct
  open Cmdliner

  let default_mime_type =
    let doc = Arg.info ~doc:"Default mime-type to serve." ["default-mime-type"] in
    Arg.(value & opt string "application/octet-stream" doc)

  let mime_type =
    let doc = Arg.info ~doc:"Overwrite mime-type for a path." ["mime-type"] in
    Arg.(value & opt_all (pair ~sep:':' string string) [] doc)

  let hook =
    let doc = Arg.info ~doc:"Webhook for pulling the repository." ["hook"] in
    Arg.(value & opt string "/hook" doc)

  let remote =
    let doc = Arg.info
        ~doc:"Remote repository url, use suffix #foo to specify a branch 'foo': \
              https://github.com/hannesm/unipi.git#gh-pages"
        ["remote"]
    in
    Arg.(required & opt (some string) None doc)

  let port =
    let doc = Arg.info ~doc:"HTTP listen port." ["port"] in
    Arg.(value & opt int 80 doc)

  let https_port =
    let doc = Arg.info ~doc:"HTTPS listen port." ["https-port"] in
    Arg.(value & opt int 443 doc)

  let tls =
    let doc = Arg.info ~doc:"Enable TLS." ["tls"] in
    Arg.(value & flag doc)

  let hostname =
    let doc = Arg.info ~doc:"Host name (used for let's encrypt and redirects)." ["hostname"] in
    Arg.(value & opt (some string) None doc)

  let production =
    let doc = Arg.info ~doc:"Let's encrypt production environment." ["production"] in
    Arg.(value & flag doc)

  let cert_seed =
    let doc = Arg.info ~doc:"Let's encrypt certificate seed." ["cert-seed"] in
    Arg.(value & opt (some string) None doc)

  let cert_key_type =
    let doc = Arg.info ~doc:"certificate key type" ["cert-key-type"] in
    Arg.(value & opt (enum X509.Key_type.strings) `RSA doc)

  let cert_bits =
    let doc = Arg.info ~doc:"certificate public key bits" ["cert-bits"] in
    Arg.(value & opt int 4096 doc)

  let account_seed =
    let doc = Arg.info ~doc:"Let's encrypt account seed." ["account-seed"] in
    Arg.(value & opt (some string) None doc)

  let account_key_type =
    let doc = Arg.info ~doc:"account key type" ["account-key-type"] in
    Arg.(value & opt (enum X509.Key_type.strings) `RSA doc)

  let account_bits =
    let doc = Arg.info ~doc:"account public key bits" ["account-bits"] in
    Arg.(value & opt int 4096 doc)

  let email =
    let doc = Arg.info ~doc:"Let's encrypt E-Mail." ["email"] in
    Arg.(value & opt (some string) None doc)

  type t = {
      mime_type: (string * string) list;
      default_mime_type: string;
      hostname: string option;
      hook: string;
      remote: string;
      tls: bool;
      production: bool;
      cert_seed: string option;
      cert_key_type: X509.Key_type.t;
      cert_bits: int;
      email: string option;
      account_seed: string option;
      account_key_type: X509.Key_type.t;
      account_bits: int;
      https_port: int;
      port: int;
    }

  let v mime_type default_mime_type hostname hook remote tls production
        cert_seed cert_key_type cert_bits email account_seed account_key_type
        account_bits https_port port
    = {
      mime_type;
      default_mime_type;
      hostname;
      hook;
      remote;
      tls;
      production;
      cert_seed;
      cert_key_type;
      cert_bits;
      email;
      account_seed;
      account_key_type;
      account_bits;
      https_port;
      port;
    }

  let setup =
    Term.(const v
          $ mime_type
          $ default_mime_type
          $ hostname
          $ hook
          $ remote
          $ tls
          $ production
          $ cert_seed
          $ cert_key_type
          $ cert_bits
          $ email
          $ account_seed
          $ account_key_type
          $ account_bits
          $ https_port
          $ port)
end

let argument_error = 64

module Main
  (_ : sig end)
  (P: Mirage_clock.PCLOCK)
  (Time: Mirage_time.S)
  (Stack: Tcpip.Stack.V4V6)
  (HTTP: Http_mirage_client.S) = struct

  module Nss = Ca_certs_nss.Make(P)
  module Paf = Paf_mirage.Make(Stack.TCP)
  module LE = LE.Make(Time)(Stack)
  module Store = Git_kv.Make(P)

  module Last_modified = struct
    let ptime_to_http_date ptime =
      let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime
      and weekday = match Ptime.weekday ptime with
        | `Mon -> "Mon" | `Tue -> "Tue" | `Wed -> "Wed" | `Thu -> "Thu"
        | `Fri -> "Fri" | `Sat -> "Sat" | `Sun -> "Sun"
      and month =
        [| "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ;
           "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov" ; "Dec" |]
    in
    let m' = Array.get month (pred m) in
    Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday d m' y hh mm ss

    (* cache the last commit (last modified and last hash) *)
    let last = ref ("", "")

    (* cache control: all resources use last-modified + etag of last commit *)
    let retrieve_last_commit store =
      Store.digest store Mirage_kv.Key.empty >>= fun last_hash ->
      Store.last_modified store Mirage_kv.Key.empty >|= fun r ->
      let v = Result.fold ~ok:Fun.id ~error:(fun _ -> Ptime.v (Pclock.now_d_ps ())) r in
      let last_date = ptime_to_http_date v in
      last := (last_date, Ohex.encode (Result.get_ok last_hash))

    let not_modified request =
      match Httpaf.Headers.get request.Httpaf.Request.headers "if-modified-since" with
      | Some ts -> String.equal ts (fst !last)
      | None -> match Httpaf.Headers.get request.Httpaf.Request.headers "if-none-match" with
        | Some etags -> List.mem (snd !last) (Astring.String.cuts ~sep:"," etags)
        | None -> false

    let last_modified () = fst !last
    let etag () = snd !last
  end

  let http_status =
    let create ~f =
      let data : (string, int) Hashtbl.t = Hashtbl.create 7 in
      (fun x ->
         let key = f x in
         let cur = match Hashtbl.find_opt data key with
           | None -> 0
           | Some x -> x
         in
         Hashtbl.replace data key (succ cur)),
      (fun () ->
         let data, total =
           Hashtbl.fold (fun key value (acc, total) ->
               (Metrics.uint key value :: acc), value + total)
             data ([], 0)
         in
         Metrics.uint "total" total :: data)
    in
    let f { Httpaf.Response.status ; _ } =
      let code = Httpaf.Status.to_code status in
      Printf.sprintf "%dxx" (code / 100)
    in
    let src =
      let open Metrics in
      let doc = "Counter metrics" in
      let incr, get = create ~f in
      let data thing = incr thing; Data.v (get ()) in
      Src.v ~doc ~tags:Metrics.Tags.[] ~data "http_response"
    in
    (fun r -> Metrics.add src (fun x -> x) (fun d -> d r))

  let respond_with_empty reqd resp =
    let hdr = Httpaf.Headers.add_unless_exists resp.Httpaf.Response.headers
      "connection" "close" in
    let resp = { resp with Httpaf.Response.headers= hdr } in
    http_status resp;
    Httpaf.Reqd.respond_with_string reqd resp ""

  module Dispatch = struct

    module M = Map.Make(String)

    let mime_type_fn mime_type default_mime_type =
      let overwrite =
        lazy (
          List.fold_left (fun acc (k, v) ->
              M.add k v acc)
            M.empty mime_type)
      and default = lazy default_mime_type
      in
      fun path ->
        let mime_type =
          match M.find_opt path (Lazy.force overwrite) with
          | Some v -> v
          | None -> Magic_mime.lookup ~default:(Lazy.force default) path
        in
        match mime_type with
        (* mime types from nginx:
           http://nginx.org/en/docs/http/ngx_http_charset_module.html#charset_types *)
        | "text/html" | "text/xml" | "text/plain" | "text/vnd.wap.wml"
        | "application/javascript" | "application/rss+xml" | "application/atom+xml"
        as content_type ->
          content_type ^ "; charset=utf-8" (* default to utf-8 *)
        | content_type -> content_type

    let dispatch mime_type store hookf hook_url _conn reqd =
      let request = Httpaf.Reqd.request reqd in
      let path =
        Uri.(pct_decode (path (of_string request.Httpaf.Request.target)))
      in
      Logs.info (fun f -> f "requested %s" path);
      if String.equal hook_url path then
        begin
          Lwt.async @@ fun () -> hookf () >>= function
          | Ok data ->
            let headers = Httpaf.Headers.of_list
              [ "content-length", string_of_int (String.length data) ] in
            let resp = Httpaf.Response.create ~headers `OK in
            http_status resp;
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | Error (`Msg msg) ->
            let headers = Httpaf.Headers.of_list
              [ "content-length", string_of_int (String.length msg) ] in
            let resp = Httpaf.Response.create ~headers `Internal_server_error in
            http_status resp;
            Httpaf.Reqd.respond_with_string reqd resp msg ;
            Lwt.return_unit
        end
      else
        if Last_modified.not_modified request then
          let resp = Httpaf.Response.create `Not_modified in
          respond_with_empty reqd resp
        else
          Lwt.async @@ fun () ->
          let find path =
            let lookup path =
              Store.get store (Mirage_kv.Key.v path)
            in
            lookup path >>= function
            | Ok r -> Lwt.return_ok (path, r)
            | Error _ ->
              let effective_path = path ^ "/index.html" in
              Lwt_result.map (fun r -> effective_path, r)
                (lookup effective_path)
          in
          find path >>= function
          | Ok (effective_path, data) ->
            let headers = [
              "content-type", mime_type effective_path ;
              "etag", Last_modified.etag () ;
              "last-modified", Last_modified.last_modified () ;
              "content-length", string_of_int (String.length data) ;
            ] in
            let headers = Httpaf.Headers.of_list headers in
            let resp = Httpaf.Response.create ~headers `OK in
            http_status resp;
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | Error _ ->
            let data = "Resource not found " ^ path in
            let headers = Httpaf.Headers.of_list
                [ "content-length", string_of_int (String.length data) ] in
            let resp = Httpaf.Response.create ~headers `Not_found in
            http_status resp;
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit

    let redirect ~hostname port _ _ reqd =
      let request = Httpaf.Reqd.request reqd in
      let response =
        Option.fold
          ~none:(
            Logs.info (fun f -> f "redirect: no host header in request");
            Httpaf.Response.create `Bad_request)
          ~some:(fun host ->
              let port = if port = 443 then None else Some port in
              let uri = Uri.of_string request.Httpaf.Request.target in
              let new_uri =
                let uri = Uri.with_host uri (Some host) in
                let uri = Uri.with_scheme uri (Some "https") in
                Uri.with_port uri port
              in
              Logs.info (fun f -> f "[%s] -> [%s]"
                            (Uri.to_string uri) (Uri.to_string new_uri));
              let headers =
                Httpaf.Headers.of_list
                  [ "location", (Uri.to_string new_uri) ] in
              Httpaf.Response.create ~headers `Moved_permanently)
          (Option.fold
             ~none:(Httpaf.Headers.get request.Httpaf.Request.headers "host")
             ~some:(fun a -> Some a)
             hostname)
      in
      respond_with_empty reqd response
  end

  let pp_error ppf = function
    | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    let resp_code = match err with
      | #Httpaf.Status.t as code -> code
      | `Exn _ -> `Internal_server_error
    in
    http_status (Httpaf.Response.create resp_code);
    Logs.err (fun m -> m "error %a while processing request %a"
                 pp_error err
                 Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum) request)

  let ( >>? ) = Lwt_result.bind

  let request_handler mime_type hook store _flow
      : _ -> Httpaf.Server_connection.request_handler
    =
    let hookf () =
      Git_kv.pull store >>= function
      | Ok [] -> Lwt.return_ok "pulled, no changes"
      | Ok _ ->
        Last_modified.retrieve_last_commit store >>= fun () ->
        Lwt.return_ok ("pulled " ^ Last_modified.etag ())
      | Error _ as e -> Lwt.return e
    in
    Dispatch.dispatch mime_type store hookf hook

  let key_type kt =
    match X509.Key_type.of_string kt with
    | Ok kt -> kt
    | Error `Msg msg ->
      Logs.err (fun m -> m "cannot decode key type %s: %s" kt msg);
      exit argument_error

  let start git_ctx () () stackv4v6 http_client
        { K.remote; tls; production; cert_seed; cert_key_type; cert_bits; email;
          account_seed; account_key_type; account_bits; hostname; mime_type;
          default_mime_type; hook; port; https_port } =
    let mime_type = Dispatch.mime_type_fn mime_type default_mime_type in
    Git_kv.connect git_ctx remote >>= fun store ->
    Last_modified.retrieve_last_commit store >>= fun () ->
    Logs.info (fun m -> m "pulled %s" (Last_modified.etag ()));
    Lwt.map
      (function Ok () -> () | Error (`Msg msg) -> failwith msg)
      (Logs.info (fun m -> m "store: %s" (Last_modified.etag ()));
       if tls then begin
         let request_handler = request_handler mime_type hook store in
         let rec provision () =
           Paf.init ~port:80 (Stack.tcp stackv4v6) >>= fun t ->
           let service =
             Paf.http_service ~error_handler (fun _ -> LE.request_handler)
           in
           let stop = Lwt_switch.create () in
           let `Initialized th0 = Paf.serve ~stop service t in
           Logs.info (fun m ->
               m "listening on 80/HTTP (let's encrypt provisioning)");
           let th1 =
             LE.provision_certificate
               ~production:production
               { LE.certificate_seed = cert_seed
               ; LE.certificate_key_type = cert_key_type
               ; LE.certificate_key_bits = Some cert_bits
               ; LE.email = Option.bind email (fun e -> Emile.of_string e |> Result.to_option)
               ; LE.account_seed = account_seed
               ; LE.account_key_type = account_key_type
               ; LE.account_key_bits = Some account_bits
               ; LE.hostname = hostname |> Option.get |> Domain_name.of_string_exn |> Domain_name.host_exn }
               http_client
               >>? fun certificates ->
             Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates in
           Lwt.both th0 th1 >>= function
           | ((), (Error _ as err)) -> Lwt.return err
           | ((), Ok certificates) ->
             Logs.debug (fun m -> m "Got certificates from let's encrypt.") ;
             let tls = Tls.Config.server ~certificates () in
             Paf.init ~port:https_port (Stack.tcp stackv4v6) >>= fun t ->
             let service =
               Paf.https_service ~tls ~error_handler request_handler
             in
             let stop = Lwt_switch.create () in
             let `Initialized th0 = Paf.serve ~stop service t in
             Logs.info (fun m -> m "listening on %d/HTTPS" port);
             Paf.init ~port (Stack.tcp stackv4v6) >>= fun t ->
             let service =
               let to_port = https_port in
               Paf.http_service ~error_handler (Dispatch.redirect ~hostname to_port)
             in
             let `Initialized th1 = Paf.serve ~stop service t in
             Logs.info (fun f -> f "listening on %d/HTTP, redirecting to %d/HTTPS" port https_port);
             Lwt.join [ th0 ; th1 ;
                        (Time.sleep_ns (Duration.of_day 80) >>= fun () -> Lwt_switch.turn_off stop) ]
               >>= fun () ->
             provision ()
         in
         provision ()
       end else begin
         let request_handler = request_handler mime_type hook store in
         Paf.init ~port (Stack.tcp stackv4v6) >>= fun t ->
         let service = Paf.http_service ~error_handler request_handler in
         let `Initialized th = Paf.serve service t in
         Logs.info (fun f -> f "listening on %d/HTTP" port);
         (th >|= fun v -> Ok v)
       end)
end
