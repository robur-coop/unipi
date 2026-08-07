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

  let hook =
    let doc = Arg.info ~doc:"Webhook for pulling the repository." ["hook"] in
    Mirage_runtime.register_arg Arg.(value & opt string "/hook" doc)

  let remote =
    let doc = Arg.info
        ~doc:"Remote repository url, use suffix #foo to specify a branch 'foo': \
              https://github.com/hannesm/unipi.git#gh-pages"
        ["remote"]
    in
    Mirage_runtime.register_arg Arg.(required & opt (some string) None doc)

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

  let default =
    let doc = Arg.info ~doc:"Redirect (with 302) to a specific URL instead of responding with not found 404." ["default"] in
    Mirage_runtime.register_arg Arg.(value & opt (some string) None doc)
end

module Main
  (_ : sig end)
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

    (* cache the last commit (last modified and last hash) *)
    let last = ref ("", "")

    (* cache control: all resources use last-modified + etag of last commit *)
    let retrieve_last_commit store =
      let commit =
        match Git_kv.commit store with
        | Some `Clean hash -> Ohex.encode (Digestif.SHA1.to_raw_string hash)
        | Some `Dirty hash ->
          Logs.warn (fun m -> m "last commit is dirty!");
          Ohex.encode (Digestif.SHA1.to_raw_string hash)
        | None -> ""
      in
      Git_kv.last_modified store Mirage_kv.Key.empty >|= fun r ->
      let v = Result.fold ~ok:Fun.id ~error:(fun _ -> Mirage_ptime.now ()) r in
      let last_date = ptime_to_http_date v in
      last := (last_date, commit)

    let not_modified request =
      match H1.Headers.get request.H1.Request.headers "if-modified-since" with
      | Some ts -> String.equal ts (fst !last)
      | None -> match H1.Headers.get request.H1.Request.headers "if-none-match" with
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

    let redirect ?(status = `Moved_permanently) reqd data =
      let headers = [
        "location", data ;
        "content-length", "0" ;
      ] in
      let headers = H1.Headers.of_list headers in
      let resp = H1.Response.create ~headers status in
      respond_with_empty reqd resp

    let extract_path req =
      if String.length req > 0 then
        if String.get req 0 = '/' then
          let last =
            match String.index_opt req ';', String.index_opt req '?' with
            | None, None -> String.length req
            | Some n, None | None, Some n -> n
            | Some a, Some b -> min a b
          in
          if last < String.length req then
            Some (String.sub req 0 last)
          else
            Some req
        else begin
          Logs.debug (fun m -> m "request path does not start with '/', but %s" req);
          None
        end
      else begin
        Logs.debug (fun m -> m "empty request path");
        None
      end

    let int_of_hex_char = function
      | '0' .. '9' as c -> Char.code c - 48
      | 'A' .. 'F' as c -> Char.code c - 55
      | 'a' .. 'f' as c -> Char.code c - 87
      | _ -> invalid_arg "not a hex char"

    let alphanum = function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
      | _ -> false

    let punctation = function
      | '.' | '-' | '_' | '~' -> true
      | _ -> false

    let percent_decode str =
      let l = String.length str in
      let b = Buffer.create l in
      let rec scan s idx =
        if idx = l then begin
          Buffer.add_substring b str s (idx - s);
          Ok ()
        end else if str.[idx] = '%' then begin
          Buffer.add_substring b str s (idx - s);
          if idx + 2 > l then
            Error "bad percent encoding (bad length)"
          else
            let high = idx + 1
            and low = idx + 2
            in
            match int_of_hex_char str.[high], int_of_hex_char str.[low] with
            | exception _ ->
              Error "bad percent encoding (invalid hex encoding)"
            | highbits, lowbits ->
              let char = Char.chr (highbits lsl 4 + lowbits) in
              if alphanum char || punctation char then
                Error "bad percent encoding (alphanumeric or punctation)"
              else begin
                Buffer.add_char b char;
                scan (low + 1) (low + 1)
              end
        end else scan s (idx + 1)
      in
      match scan 0 0 with
      | Ok () -> Ok (Buffer.contents b)
      | Error _ as e -> e

    let path_of_target target reqd k =
      let bad_request () = respond_with_empty reqd (H1.Response.create `Bad_request) in
      match extract_path target with
      | None ->
        bad_request ()
      | Some path ->
        match percent_decode path with
        | Error msg ->
          Logs.debug (fun m -> m "Bad path: %s" msg);
          respond_with_empty reqd (H1.Response.create `Bad_request)
        | Ok path ->
          k path

    let dispatch mime_type store hookf hook_url _conn reqd =
      let request = H1.Reqd.request reqd in
      path_of_target request.H1.Request.target reqd @@ fun path ->
      Logs.info (fun f -> f "requested %s" path);
      if String.equal hook_url path then
        begin
          Lwt.async @@ fun () -> hookf () >>= function
          | Ok data ->
            let headers = H1.Headers.of_list
                [ "content-length", string_of_int (String.length data) ] in
            let resp = H1.Response.create ~headers `OK in
            http_status resp;
            H1.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | Error (`Msg msg) ->
            let headers = H1.Headers.of_list
                [ "content-length", string_of_int (String.length msg) ] in
            let resp = H1.Response.create ~headers `Internal_server_error in
            http_status resp;
            H1.Reqd.respond_with_string reqd resp msg ;
            Lwt.return_unit
        end
      else
      if Last_modified.not_modified request then
        let resp = H1.Response.create `Not_modified in
        respond_with_empty reqd resp
      else
        Lwt.async @@ fun () ->
        let find path =
          let lookup path =
            Git_kv.get_with_permissions store (Mirage_kv.Key.v path)
          in
          lookup path >>= function
          | Ok (perm, data) -> Lwt.return_ok (path, perm, data)
          | Error _ ->
            let effective_path = path ^ "/index.html" in
            Lwt_result.map (fun (perm, data) -> effective_path, perm, data)
              (lookup effective_path)
        in
        find path >>= function
        | Ok (effective_path, `Link, data) ->
          redirect reqd data;
          Lwt.return_unit
        | Ok (effective_path, _perm, data) ->
          let headers = [
            "content-type", mime_type effective_path ;
            "etag", Last_modified.etag () ;
            "last-modified", Last_modified.last_modified () ;
            "content-length", string_of_int (String.length data) ;
          ] in
          let headers = H1.Headers.of_list headers in
          let resp = H1.Response.create ~headers `OK in
          http_status resp;
          H1.Reqd.respond_with_string reqd resp data ;
          Lwt.return_unit
        | Error _ ->
          match K.default () with
          | Some url ->
            redirect ~status:`Found reqd url;
            Lwt.return_unit
          | None ->
            let data = "Resource not found " ^ path in
            let headers = H1.Headers.of_list
                [ "content-length", string_of_int (String.length data) ] in
            let resp = H1.Response.create ~headers `Not_found in
            http_status resp;
            H1.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit

    let redirect ~hostname port _ _ reqd =
      let request = H1.Reqd.request reqd in
      let host =
        Option.fold
          ~none:(H1.Headers.get request.H1.Request.headers "host")
          ~some:(fun a -> Some a)
          hostname
      in
      match host with
      | None ->
        Logs.info (fun f -> f "redirect: no host header in request");
        let response = H1.Response.create `Bad_request in
        respond_with_empty reqd response
      | Some host ->
        let port = if port = 443 then "" else ":" ^ string_of_int port in
        let new_uri =
          String.concat "" [ "https://" ; host ; port ; request.H1.Request.target ]
        in
        Logs.info (fun f -> f "[%s] -> [%s]" request.H1.Request.target new_uri);
        redirect reqd new_uri
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

  let request_handler mime_type hook store _flow
      : _ -> H1.Server_connection.request_handler
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
      exit Mirage_runtime.argument_error

  let start git_ctx stackv4v6 http_client =
    let mime_type = Dispatch.mime_type_fn (K.mime_type ()) (K.default_mime_type ()) in
    Git_kv.connect git_ctx (K.remote ()) >>= fun store ->
    Last_modified.retrieve_last_commit store >>= fun () ->
    Logs.info (fun m -> m "pulled %s" (Last_modified.etag ()));
    Lwt.map
      (function
        | Ok () -> ()
        | Error (`Msg msg) -> failwith msg
        | Error (`HTTP err) -> failwith (Fmt.to_to_string Mimic.pp_error err)
      )
      (Logs.info (fun m -> m "store: %s" (Last_modified.etag ()));
       if K.tls () then begin
         let request_handler = request_handler mime_type (K.hook ()) store in
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
         let request_handler = request_handler mime_type (K.hook ()) store in
         Paf.init ~port:(K.port ()) (Stack.tcp stackv4v6) >>= fun t ->
         let service = Paf.http_service ~error_handler request_handler in
         let `Initialized th = Paf.serve service t in
         Logs.info (fun f -> f "listening on %d/HTTP" (K.port ()));
         (th >|= fun v -> Ok v)
       end)
end
