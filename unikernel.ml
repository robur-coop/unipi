open Lwt.Infix

module Main (S: Mirage_stack.V4) (RES: Resolver_lwt.S) (CON: Conduit_mirage.S) (C: Mirage_clock.PCLOCK) = struct

  module Http = Cohttp_mirage.Server_with_conduit

  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let decompose_git_url () =
    match String.split_on_char '#' (Key_gen.remote ()) with
    | [ url ] -> (url, None)
    | [ url ; branch ] -> (url, Some branch)
    | _ -> invalid_arg "expected at most a single # in remote"

  let connect_store resolver conduit =
    let uri, branch = decompose_git_url () in
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= fun r ->
    (match branch with
     | None -> Store.master r
     | Some branch -> Store.of_branch r branch) >|= fun repo ->
    repo, Store.remote ~conduit ~resolver uri

  let ptime_to_http_date ptime =
    let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time ptime
    and weekday = match Ptime.weekday ptime with
      | `Mon -> "Mon" | `Tue -> "Tue" | `Wed -> "Wed" | `Thu -> "Thu"
      | `Fri -> "Fri" | `Sat -> "Sat" | `Sun -> "Sun"
    and month =
      [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|]
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
        | None -> Ptime.v (C.now_d_ps ())
        | Some d -> d
      in
      ptime_to_http_date ptime
    and last_commit_hash =
      Fmt.to_to_string (Irmin.Type.pp Store.Hash.t) (Store.Commit.hash head)
    in
    last := (last_commit_date, last_commit_hash)

  let pull_store store upstream =
    Logs.info (fun m -> m "pulling from remote!");
    Sync.pull store upstream `Set >>= fun r ->
    retrieve_last_commit store >|= fun () ->
    match r with
    | Ok (`Head _ as s) -> Ok (Fmt.strf "pulled %a" Sync.pp_status s)
    | Ok `Empty -> Error (`Msg "pulled empty repository")
    | Error (`Msg e) -> Error (`Msg ("pull error " ^ e))
    | Error (`Conflict msg) -> Error (`Msg ("pull conflict " ^ msg))

  let not_modified request =
    let hdr = request.Cohttp.Request.headers in
    match Cohttp.Header.get hdr "if-modified-since" with
    | Some ts -> String.equal ts (fst !last)
    | None -> match Cohttp.Header.get hdr "if-none-match" with
      | Some etags -> List.mem (snd !last) (Astring.String.cuts ~sep:"," etags)
      | None -> false

  let dispatch store hook request _body =
    let p = Uri.path (Cohttp.Request.uri request) in
    let path = if String.equal p "/" then "index.html" else p in
    match Astring.String.cuts ~sep:"/" ~empty:false path with
    | [ h ] when String.equal (Key_gen.hook ()) h ->
      begin
        hook () >>= function
        | Ok data -> Http.respond ~status:`OK ~body:(`String data) ()
        | Error (`Msg msg) ->
          Http.respond ~status:`Internal_server_error ~body:(`String msg) ()
      end
    | path_list ->
      if not_modified request then
        Http.respond ~status:`Not_modified ~body:`Empty ()
      else
        Store.find store path_list >>= function
        | Some data ->
          let mime_type = Magic_mime.lookup path in
          let headers = [
            "content-type", mime_type ;
            "etag", snd !last ;
            "last-modified", fst !last
          ] in
          let headers = Cohttp.Header.of_list headers in
          Http.respond ~headers ~status:`OK ~body:(`String data) ()
        | None ->
          let data = "Resource not found " ^ path in
          Http.respond ~status:`Not_found ~body:(`String data) ()

  let start stack resolver conduit () =
    connect_store resolver conduit >>= fun (store, upstream) ->
    pull_store store upstream >>= function
    | Error (`Msg msg) -> Lwt.fail_with msg
    | Ok data ->
      Logs.info (fun m -> m "%s" data);
      Http.connect conduit >>= fun http ->
      let http_port = Key_gen.port () in
      let tcp = `TCP http_port in
      let hook () = pull_store store upstream in
      let serve cb =
        let callback _ request body = cb request body
        and conn_closed _ = ()
        in
        Http.make ~conn_closed ~callback ()
      in
      let http =
        Logs.info (fun f -> f "listening on %d/TCP" http_port);
        http tcp (serve (dispatch store hook))
      in
      http
end
