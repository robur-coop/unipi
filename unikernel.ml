open Lwt.Infix

let argument_error = 64

module Main
  (_ : sig end)
  (Random: Mirage_random.S)
  (M: Mirage_clock.MCLOCK)
  (P: Mirage_clock.PCLOCK)
  (Time: Mirage_time.S)
  (Stack: Tcpip.Stack.V4V6) = struct

  module Nss = Ca_certs_nss.Make(P)
  module Paf = Paf_mirage.Make(Stack.TCP)
  module LE = LE.Make(Time)(Stack)
  module DNS = Dns_client_mirage.Make(Random)(Time)(M)(P)(Stack)
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
      let v = Result.fold ~ok:Fun.id ~error:(fun _ -> Pclock.now_d_ps ()) r in
      let last_date = ptime_to_http_date (Ptime.v v) in
      last := (last_date, Result.get_ok last_hash)

    let not_modified request =
      match Httpaf.Headers.get request.Httpaf.Request.headers "if-modified-since" with
      | Some ts -> String.equal ts (fst !last)
      | None -> match Httpaf.Headers.get request.Httpaf.Request.headers "if-none-match" with
        | Some etags -> List.mem (snd !last) (Astring.String.cuts ~sep:"," etags)
        | None -> false

    let last_modified () = fst !last
    let etag () = snd !last
  end

  let respond_with_empty reqd resp =
    let hdr = Httpaf.Headers.add_unless_exists resp.Httpaf.Response.headers
      "connection" "close" in
    let resp = { resp with Httpaf.Response.headers= hdr } in
    Httpaf.Reqd.respond_with_string reqd resp ""

  module Dispatch = struct

    module M = Map.Make(String)

    let mime_type =
      let overwrite =
        lazy (
          List.fold_left (fun acc (k, v) ->
              M.add k v acc)
            M.empty (Key_gen.mime_type ()))
      and default =
        lazy (Key_gen.default_mime_type ())
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

    let dispatch store hookf hook_url _conn reqd =
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
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | Error (`Msg msg) ->
            let headers = Httpaf.Headers.of_list
              [ "content-length", string_of_int (String.length msg) ] in
            let resp = Httpaf.Response.create ~headers `Internal_server_error in
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
            | Ok _ as r -> Lwt.return r
            | Error _ -> lookup (path ^ "/index.html")
          in
          find path >>= function
          | Ok data ->
            let headers = [
              "content-type", mime_type path ;
              "etag", Last_modified.etag () ;
              "last-modified", Last_modified.last_modified () ;
              "content-length", string_of_int (String.length data) ;
            ] in
            let headers = Httpaf.Headers.of_list headers in
            let resp = Httpaf.Response.create ~headers `OK in
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | Error _ ->
            let data = "Resource not found " ^ path in
            let headers = Httpaf.Headers.of_list
                [ "content-length", string_of_int (String.length data) ] in
            let resp = Httpaf.Response.create ~headers `Not_found in
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit

    let redirect port _ _ reqd =
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
             (Key_gen.hostname ()))
      in
      respond_with_empty reqd response
  end

  let pp_error ppf = function
    | #Httpaf.Status.t as code -> Httpaf.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    Logs.err (fun m -> m "error %a while processing request %a"
                 pp_error err
                 Fmt.(option ~none:(any "unknown") Httpaf.Request.pp_hum) request)

  let ( >>? ) = Lwt_result.bind

  let request_handler store _flow : _ -> Httpaf.Server_connection.request_handler =
    let hookf () =
      Git_kv.pull store >>= function
      | Ok [] -> Lwt.return_ok "pulled, no changes"
      | Ok _ ->
        Last_modified.retrieve_last_commit store >>= fun () ->
        Lwt.return_ok ("pulled " ^ Last_modified.etag ())
      | Error _ as e -> Lwt.return e
    in
    Dispatch.dispatch store hookf (Key_gen.hook ())

  let key_type kt =
    match X509.Key_type.of_string kt with
    | Ok kt -> kt
    | Error `Msg msg ->
      Logs.err (fun m -> m "cannot decode key type %s: %s" kt msg);
      exit argument_error

  let start git_ctx () () () () stackv4v6 =
    Git_kv.connect git_ctx (Key_gen.remote ()) >>= fun store ->
    Last_modified.retrieve_last_commit store >>= fun () ->
    Logs.info (fun m -> m "pulled %s" (Last_modified.etag ()));
    Lwt.map
      (function Ok () -> Lwt.return_unit | Error (`Msg msg) -> Lwt.fail_with msg)
      (Logs.info (fun m -> m "store: %s" (Last_modified.etag ()));
       if Key_gen.tls () then begin
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
             let gethostbyname dns domain_name =
               DNS.gethostbyname dns domain_name >>? fun ipv4 ->
               Lwt.return_ok (Ipaddr.V4 ipv4)
             in
             LE.provision_certificate
               ~production:(Key_gen.production ())
               { LE.certificate_seed = Key_gen.cert_seed ()
               ; LE.certificate_key_type = key_type (Key_gen.cert_key_type ())
               ; LE.certificate_key_bits = Some (Key_gen.cert_bits ())
               ; LE.email = Option.bind (Key_gen.email ()) (fun e -> Emile.of_string e |> Result.to_option)
               ; LE.account_seed = Key_gen.account_seed ()
               ; LE.account_key_type = key_type (Key_gen.account_key_type ())
               ; LE.account_key_bits = Some (Key_gen.account_bits ())
               ; LE.hostname = Key_gen.hostname () |> Option.get |> Domain_name.of_string_exn |> Domain_name.host_exn }
               (LE.ctx
                  ~gethostbyname
                  ~authenticator:(Result.get_ok (Nss.authenticator ()))
                  (DNS.create stackv4v6) stackv4v6)
               >>? fun certificates ->
             Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates in
           Lwt.both th0 th1 >>= function
           | ((), (Error _ as err)) -> Lwt.return err
           | ((), Ok certificates) ->
             Logs.debug (fun m -> m "Got certificates from let's encrypt.") ;
             let tls = Tls.Config.server ~certificates () in
             Paf.init ~port:(Key_gen.https_port ()) (Stack.tcp stackv4v6) >>= fun t ->
             let service =
               Paf.https_service ~tls ~error_handler (request_handler store)
             in
             let stop = Lwt_switch.create () in
             let `Initialized th0 = Paf.serve ~stop service t in
             Logs.info (fun m ->
                 m "listening on %d/HTTPS" (Key_gen.port ()));
             Paf.init ~port:(Key_gen.port ()) (Stack.tcp stackv4v6) >>= fun t ->
             let service =
               let to_port = (Key_gen.https_port ()) in
               Paf.http_service ~error_handler (Dispatch.redirect to_port)
             in
             let `Initialized th1 = Paf.serve ~stop service t in
             Logs.info (fun f -> f "listening on %d/HTTP, redirecting to %d/HTTPS"
                           (Key_gen.port ()) (Key_gen.https_port ()));
             Lwt.join [ th0 ; th1 ;
                        (Time.sleep_ns (Duration.of_day 80) >>= fun () -> Lwt_switch.turn_off stop) ]
               >>= fun () ->
             provision ()
         in
         provision ()
       end else begin
         Paf.init ~port:(Key_gen.port ()) (Stack.tcp stackv4v6) >>= fun t ->
         let service =
           Paf.http_service ~error_handler (request_handler store)
         in
         let `Initialized th = Paf.serve service t in
         Logs.info (fun f -> f "listening on %d/HTTP" (Key_gen.port ()));
         (th >|= fun v -> Ok v)
       end)
end
