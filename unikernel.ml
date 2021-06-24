[@@@warning "-45"]

open Lwt.Infix

let argument_error = 64
let ( <.> ) f g = fun x -> f (g x)

module Main
  (_ : sig end)
  (Random: Mirage_random.S)
  (M: Mirage_clock.MCLOCK)
  (P: Mirage_clock.PCLOCK)
  (Time: Mirage_time.S)
  (Stack: Mirage_stack.V4V6) = struct

  module Nss = Ca_certs_nss.Make(P)
  module Paf = Paf_mirage.Make(Time)(Stack)
  module LE = LE.Make(Time)(Stack)
  module DNS = Dns_client_mirage.Make(Random)(Time)(M)(Stack)
  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

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
      Store.Head.get store >|= fun head ->
      let last_commit_date =
        let info = Store.Commit.info head in
        let ptime =
          match Ptime.of_float_s (Int64.to_float (Irmin.Info.date info)) with
          | None -> Ptime.v (P.now_d_ps ())
          | Some d -> d
        in
        ptime_to_http_date ptime
      and last_commit_hash =
        Fmt.to_to_string (Irmin.Type.pp Store.Hash.t) (Store.Commit.hash head)
      in
      last := (last_commit_date, last_commit_hash)

    let not_modified request =
      match Httpaf.Headers.get request.Httpaf.Request.headers "if-modified-since" with
      | Some ts -> String.equal ts (fst !last)
      | None -> match Httpaf.Headers.get request.Httpaf.Request.headers "if-none-match" with
        | Some etags -> List.mem (snd !last) (Astring.String.cuts ~sep:"," etags)
        | None -> false

    let last_modified () = fst !last
    let etag () = snd !last
  end

  module Remote = struct
    let decompose_git_url () =
      match String.split_on_char '#' (Key_gen.remote ()) with
      | [ url ] -> url, None
      | [ url ; branch ] -> url, Some branch
      | _ ->
        Logs.err (fun m -> m "expected at most a single # in remote");
        exit argument_error

    let connect ctx =
      let uri, branch = decompose_git_url () in
      let config = Irmin_mem.config () in
      Store.Repo.v config >>= fun r ->
      (match branch with
       | None -> Store.master r
       | Some branch -> Store.of_branch r branch) >|= fun repo ->
      repo, Store.remote ~ctx uri

    let pull store upstream =
      Logs.info (fun m -> m "pulling from remote!");
      Sync.pull ~depth:1 store upstream `Set >>= fun r ->
      Last_modified.retrieve_last_commit store >|= fun () ->
      match r with
      | Ok (`Head _ as s) -> Ok (Fmt.strf "pulled %a" Sync.pp_status s)
      | Ok `Empty -> Error (`Msg "pulled empty repository")
      | Error (`Msg e) -> Error (`Msg ("pull error " ^ e))
      | Error (`Conflict msg) -> Error (`Msg ("pull conflict " ^ msg))
  end

  let respond_with_empty reqd resp =
    let hdr = Httpaf.Headers.add_unless_exists resp.Httpaf.Response.headers
      "connection" "close" in
    let resp = { resp with Httpaf.Response.headers= hdr } in
    Httpaf.Reqd.respond_with_string reqd resp ""

  module Dispatch = struct
    let dispatch store hookf hook_url _conn reqd =
      let request = Httpaf.Reqd.request reqd in
      let path = request.Httpaf.Request.target in
      let path = if String.equal path "/" then "index.html" else path in
      Logs.info (fun f -> f "requested %s" path);
      match Astring.String.cuts ~sep:"/" ~empty:false path with
      | [ h ] when String.equal hook_url h ->
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
      | path_list ->
        if Last_modified.not_modified request then
          let resp = Httpaf.Response.create `Not_modified in
          respond_with_empty reqd resp
        else
          Lwt.async @@ fun () -> Store.find store path_list >>= function
          | Some data ->
            let mime_type = Magic_mime.lookup path in (* TODO(dinosaure): replace by conan. *)
            let headers = [
              "content-type", mime_type ;
              "etag", Last_modified.etag () ;
              "last-modified", Last_modified.last_modified () ;
              "content-length", string_of_int (String.length data) ;
            ] in
            let headers = Httpaf.Headers.of_list headers in
            let resp = Httpaf.Response.create ~headers `OK in
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit
          | None ->
            let data = "Resource not found " ^ path in
            let headers = Httpaf.Headers.of_list
              [ "content-length", string_of_int (String.length data) ] in
            let resp = Httpaf.Response.create ~headers `Not_found in
            Httpaf.Reqd.respond_with_string reqd resp data ;
            Lwt.return_unit

    let redirect port _ reqd =
      let request = Httpaf.Reqd.request reqd in
      let port = if port = 443 then None else Some port in
      let path = request.Httpaf.Request.target in
      let new_uri = Uri.make ~scheme:"https" ?host:(Key_gen.hostname ()) ?port ~path () in
      (* TODO(dinosaure): check it. *)
      Logs.info (fun f -> f "[%s] -> [%s]"
                    path (Uri.to_string new_uri));
      let headers =
        Httpaf.Headers.of_list
          [ "location", (Uri.to_string new_uri) ] in
      let resp = Httpaf.Response.create ~headers `Moved_permanently in
      respond_with_empty reqd resp
  end

  let ignore_error _ ?request:_ _ _ = ()
  let ( >>? ) = Lwt_result.bind

  let request_handler store upstream : _ -> Httpaf.Server_connection.request_handler =
    let hook_url = Key_gen.hook () in
    if Astring.String.is_infix ~affix:"/" hook_url then begin
      Logs.err (fun m -> m "hook url contains /, which is not allowed");
      exit argument_error
    end else
      let hookf () = Remote.pull store upstream in
      Dispatch.dispatch store hookf hook_url

  let start git_ctx () () () () stackv4v6 =
    Remote.connect git_ctx >>= fun (store, upstream) ->
    Lwt.map
      (function Ok () -> Lwt.return_unit | Error (`Msg msg) -> Lwt.fail_with msg)
      (Remote.pull store upstream >>? fun data ->
       Logs.info (fun m -> m "store: %s" data);
       if Key_gen.tls () then begin
         let rec provision () =
           Paf.init ~port:80 stackv4v6 >>= fun t ->
           let service = Paf.http_service
             ~error_handler:ignore_error
             LE.request_handler in
           let stop = Lwt_switch.create () in
           let `Initialized th0 = Paf.serve ~stop service t in
           Logs.info (fun m ->
               m "listening on 80/HTTP (let's encrypt provisioning)");
           let th1 =
             LE.provision_certificate
               ~production:(Key_gen.production ())
               { LE.certificate_seed= Key_gen.cert_seed ()
               ; LE.email= Option.bind (Key_gen.email ()) (Rresult.R.to_option <.> Emile.of_string)
               ; LE.seed= Key_gen.account_seed ()
               ; LE.hostname= (Domain_name.(host_exn <.> of_string_exn <.> Option.get) (Key_gen.hostname ())) }
               (LE.ctx
                  ~gethostbyname:(fun dns domain_name -> DNS.gethostbyname dns domain_name >>? fun ipv4 -> Lwt.return_ok (Ipaddr.V4 ipv4))
                  ~authenticator:(Rresult.R.failwith_error_msg (Nss.authenticator ()))
                  (DNS.create stackv4v6) stackv4v6)
               >>? fun certificates ->
             Lwt_switch.turn_off stop >>= fun () -> Lwt.return_ok certificates in
           Lwt.both th0 th1 >>= function
           | ((), (Error _ as err)) -> Lwt.return err
           | ((), Ok certificates) ->
             Logs.debug (fun m -> m "Got certificates from let's encrypt.") ;
             let tls = Tls.Config.server ~certificates () in
             Paf.init ~port:(Key_gen.port ()) stackv4v6 >>= fun t ->
             let service = Paf.https_service ~tls
               ~error_handler:ignore_error
               (request_handler store upstream) in
             let stop = Lwt_switch.create () in
             let `Initialized th = Paf.serve ~stop service t in
             Logs.info (fun m ->
                 m "listening on %d/HTTPS" (Key_gen.port ()));
             Lwt.both th
               (Time.sleep_ns (Duration.of_day 80) >>= fun () -> Lwt_switch.turn_off stop)
               >>= fun ((), ()) ->
             provision ()
         in
         provision ()
       end else begin
         Paf.init ~port:(Key_gen.port ()) stackv4v6 >>= fun t ->
         let service = Paf.http_service
           ~error_handler:ignore_error
           (request_handler store upstream) in
         let `Initialized th = Paf.serve service t in
         Logs.info (fun f -> f "listening on %d/HTTP" (Key_gen.port ()));
         (th >|= fun v -> Ok v)
       end)
end
