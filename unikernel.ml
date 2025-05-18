open Lwt.Infix

module K = struct
  open Cmdliner

  let default_mime_type =
    let doc = Arg.info ~doc:"Default mime-type to serve." ["default-mime-type"] in
    Mirage_runtime.register_arg
      Arg.(value & opt string "application/octet-stream" doc)

  let mime_type =
    let doc = Arg.info ~doc:"Overwrite mime-type for a path." ["mime-type"] in
    Mirage_runtime.register_arg
      Arg.(value & opt_all (pair ~sep:':' string string) [] doc)

  let port =
    let doc = Arg.info ~doc:"HTTP listen port." ["port"] in
    Mirage_runtime.register_arg Arg.(value & opt int 80 doc)

  let https_port =
    let doc = Arg.info ~doc:"HTTPS listen port." ["https-port"] in
    Mirage_runtime.register_arg Arg.(value & opt int 443 doc)

  let tls =
    let doc = Arg.info ~doc:"Enable TLS." ["tls"] in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let hostname =
    let doc = Arg.info ~doc:"Host name (used for let's encrypt and redirects)." ["hostname"] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)

  let production =
    let doc = Arg.info ~doc:"Let's encrypt production environment." ["production"] in
    Mirage_runtime.register_arg Arg.(value & flag doc)

  let cert_seed =
    let doc = Arg.info ~doc:"Let's encrypt certificate seed." ["cert-seed"] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)

  let cert_key_type =
    let doc = Arg.info ~doc:"certificate key type" ["cert-key-type"] in
    Mirage_runtime.register_arg
      Arg.(value & opt (enum X509.Key_type.strings) `RSA doc)

  let cert_bits =
    let doc = Arg.info ~doc:"certificate public key bits" ["cert-bits"] in
    Mirage_runtime.register_arg Arg.(value & opt int 4096 doc)

  let account_seed =
    let doc = Arg.info ~doc:"Let's encrypt account seed." ["account-seed"] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)

  let account_key_type =
    let doc = Arg.info ~doc:"account key type" ["account-key-type"] in
    Mirage_runtime.register_arg
      Arg.(value & opt (enum X509.Key_type.strings) `RSA doc)

  let account_bits =
    let doc = Arg.info ~doc:"account public key bits" ["account-bits"] in
    Mirage_runtime.register_arg Arg.(value & opt int 4096 doc)

  let email =
    let doc = Arg.info ~doc:"Let's encrypt E-Mail." ["email"] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)
end

module Main
  (Store : Mirage_kv.RO)
  (Stack: Tcpip.Stack.V4V6)
  (HTTP: Http_mirage_client.S) = struct

  module Paf = Paf_mirage.Make(Stack.TCP)
  module LE = LE.Make(Stack)

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

    let not_modified last_modified request =
      match H1.Headers.get request.H1.Request.headers "if-modified-since" with
      | Some ts -> String.equal ts (ptime_to_http_date last_modified)
      | None -> false
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
    let f { H1.Response.status ; _ } =
      let code = H1.Status.to_code status in
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
    let hdr = H1.Headers.add_unless_exists resp.H1.Response.headers
      "connection" "close" in
    let resp = { resp with H1.Response.headers= hdr } in
    http_status resp;
    H1.Reqd.respond_with_string reqd resp ""

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

    let dispatch mime_type store _conn reqd =
      let request = H1.Reqd.request reqd in
      let path =
        Uri.(pct_decode (path (of_string request.H1.Request.target)))
      in
      Logs.info (fun f -> f "requested %s" path);
      Lwt.async @@ fun () ->
      let find path =
        let lookup path =
          let k = Mirage_kv.Key.v path in
          let open Lwt_result.Syntax in
          let* size = Store.size store k in
          let+ last_modified = Store.last_modified store k in
          (last_modified, size)
        in
        lookup path >>= function
        | Ok (last_modified, size) -> Lwt.return_ok (path, last_modified, size)
        | Error _ ->
          let effective_path = path ^ "/index.html" in
          Lwt_result.map (fun (last_modified, size) -> effective_path, last_modified, size)
            (lookup effective_path)
      in
      find path >>= function
      | Ok (effective_path, last_modified, size) ->
        if Last_modified.not_modified last_modified request then
          let resp = H1.Response.create `Not_modified in
          respond_with_empty reqd resp ;
          Lwt.return_unit
        else
          let headers = [
            "content-type", mime_type effective_path ;
            (* "etag", Last_modified.etag () ; *)
            "last-modified", Last_modified.ptime_to_http_date last_modified ;
            "content-length", Optint.Int63.to_string size ;
          ] in
          let headers = H1.Headers.of_list headers in
          let resp = H1.Response.create ~headers `OK in
          http_status resp;
          let stream =
            H1.Reqd.respond_with_streaming
              ~flush_headers_immediately:true
              reqd resp
          in
          let rec loop offset =
            let length = Optint.Int63.(to_int (min (of_int 16384) (sub size offset))) in
            if length <= 0 then (H1.Body.Writer.close stream; Lwt.return_unit)
            else
              Store.get_partial store
                (Mirage_kv.Key.v effective_path)
                ~offset ~length
              >>= function
              | Ok data ->
                H1.Body.Writer.write_string stream data;
                let continue, wakeup = Lwt.task () in
                H1.Body.Writer.flush_with_reason stream
                  (function
                    | `Closed ->
                      Logs.warn (fun m -> m "Closed while handling %S" effective_path);
                      Lwt.wakeup wakeup `Closed
                    | `Written -> Lwt.wakeup wakeup `Continue);
                continue >>= (function
                    | `Closed -> H1.Body.Writer.close stream; Lwt.return_unit
                    | `Continue -> loop (Optint.Int63.(add offset (of_int length))))
              | Error e ->
                Logs.warn (fun m -> m "Error reading %s: %a"
                              effective_path
                              Store.pp_error e);
                H1.Body.Writer.close stream;
                Lwt.return_unit
          in
          loop Optint.Int63.zero
      | Error _ ->
        let data = "Resource not found " ^ path in
        let headers = H1.Headers.of_list
            [ "content-length", string_of_int (String.length data) ] in
        let resp = H1.Response.create ~headers `Not_found in
        http_status resp;
        H1.Reqd.respond_with_string reqd resp data ;
        Lwt.return_unit

    let redirect ~hostname port _ _ reqd =
      let request = H1.Reqd.request reqd in
      let response =
        Option.fold
          ~none:(
            Logs.info (fun f -> f "redirect: no host header in request");
            H1.Response.create `Bad_request)
          ~some:(fun host ->
              let port = if port = 443 then None else Some port in
              let uri = Uri.of_string request.H1.Request.target in
              let new_uri =
                let uri = Uri.with_host uri (Some host) in
                let uri = Uri.with_scheme uri (Some "https") in
                Uri.with_port uri port
              in
              Logs.info (fun f -> f "[%s] -> [%s]"
                            (Uri.to_string uri) (Uri.to_string new_uri));
              let headers =
                H1.Headers.of_list
                  [ "location", (Uri.to_string new_uri) ] in
              H1.Response.create ~headers `Moved_permanently)
          (Option.fold
             ~none:(H1.Headers.get request.H1.Request.headers "host")
             ~some:(fun a -> Some a)
             hostname)
      in
      respond_with_empty reqd response
  end

  let pp_error ppf = function
    | #H1.Status.t as code -> H1.Status.pp_hum ppf code
    | `Exn exn -> Fmt.pf ppf "exception %s" (Printexc.to_string exn)

  let error_handler _dst ?request err _ =
    let resp_code = match err with
      | #H1.Status.t as code -> code
      | `Exn _ -> `Internal_server_error
    in
    http_status (H1.Response.create resp_code);
    Logs.err (fun m -> m "error %a while processing request %a"
                 pp_error err
                 Fmt.(option ~none:(any "unknown") H1.Request.pp_hum) request)

  let ( >>? ) = Lwt_result.bind

  let request_handler mime_type store _flow
      : _ -> H1.Server_connection.request_handler
    =
    Dispatch.dispatch mime_type store

  let key_type kt =
    match X509.Key_type.of_string kt with
    | Ok kt -> kt
    | Error `Msg msg ->
      Logs.err (fun m -> m "cannot decode key type %s: %s" kt msg);
      exit Mirage_runtime.argument_error

  let start store stackv4v6 http_client =
    let mime_type = Dispatch.mime_type_fn (K.mime_type ()) (K.default_mime_type ()) in

    Lwt.map
      (function Ok () -> () | Error (`Msg msg) -> failwith msg)
      (if K.tls () then begin
         let request_handler = request_handler mime_type store in
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
               ~production:(K.production ())
               { LE.certificate_seed = K.cert_seed ()
               ; LE.certificate_key_type = K.cert_key_type ()
               ; LE.certificate_key_bits = Some (K.cert_bits ())
               ; LE.email = Option.bind (K.email ()) (fun e -> Emile.of_string e |> Result.to_option)
               ; LE.account_seed = K.account_seed ()
               ; LE.account_key_type = K.account_key_type ()
               ; LE.account_key_bits = Some (K.account_bits ())
               ; LE.hostname = K.hostname () |> Option.get |> Domain_name.of_string_exn |> Domain_name.host_exn }
               http_client
               >>? fun certificates ->
             Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates in
           Lwt.both th0 th1 >>= function
           | ((), (Error _ as err)) -> Lwt.return err
           | ((), Ok certificates) ->
             Logs.debug (fun m -> m "Got certificates from let's encrypt.") ;
             match Tls.Config.server ~certificates () with
             | Error `Msg msg as err ->
               Logs.err (fun m -> m "Couldn't construct the TLS configuration: %s" msg);
               Lwt.return err
             | Ok tls ->
               Paf.init ~port:(K.https_port ()) (Stack.tcp stackv4v6) >>= fun t ->
               let service =
                 Paf.https_service ~tls ~error_handler request_handler
               in
               let stop = Lwt_switch.create () in
               let `Initialized th0 = Paf.serve ~stop service t in
               Logs.info (fun m -> m "listening on %d/HTTPS" (K.port ()));
               Paf.init ~port:(K.port ()) (Stack.tcp stackv4v6) >>= fun t ->
               let service =
                 let to_port = K.https_port () in
                 Paf.http_service ~error_handler (Dispatch.redirect ~hostname:(K.hostname ()) to_port)
               in
               let `Initialized th1 = Paf.serve ~stop service t in
               Logs.info (fun f -> f "listening on %d/HTTP, redirecting to %d/HTTPS" (K.port ()) (K.https_port ()));
               Lwt.join [ th0 ; th1 ;
                          (Mirage_sleep.ns (Duration.of_day 80) >>= fun () -> Lwt_switch.turn_off stop) ]
               >>= fun () ->
               provision ()
         in
         provision ()
       end else begin
         let request_handler = request_handler mime_type store in
         Paf.init ~port:(K.port ()) (Stack.tcp stackv4v6) >>= fun t ->
         let service = Paf.http_service ~error_handler request_handler in
         let `Initialized th = Paf.serve service t in
         Logs.info (fun f -> f "listening on %d/HTTP" (K.port ()));
         (th >|= fun v -> Ok v)
       end)
end
