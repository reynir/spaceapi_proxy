open Lwt.Infix

let spaceapi_url = Uri.of_string "http://spaceapi.osaa.dk/status/json"

let get () =
  Cohttp_lwt_unix.Client.get
    spaceapi_url
  >>= fun (response, body) ->
  match response.Cohttp.Response.status with
  | `OK ->
    Cohttp_lwt_body.to_string body
    |> Lwt_result.ok
  | _ ->
    Cohttp_lwt_body.to_string body
    >>= fun body ->
    Lwt_result.fail (Printf.sprintf "Got HTTP status code %d"
                       (Cohttp.Code.code_of_status response.Cohttp.Response.status))

let get_state () =
  let open Lwt_result.Infix in
  get () >>= fun json_string ->
  match Yojson.Safe.from_string json_string with
  | `Assoc members ->
    (List.assoc "state" members
     |> function 
     | `Assoc [("open", `Bool state)] ->
       Lwt_result.return state
     | _ ->
       Lwt_result.fail "Bad json")
  | _ ->
    Lwt_result.fail "Bad json"

let mk_state_stream () : bool Lwt_stream.t  =
  let stream, push = Lwt_stream.create () in
  let rec loop () =
    get_state () >>= fun s ->
    match s with
    | Ok state ->
      push (Some state);
      Lwt_unix.sleep 1.0 >>=
      loop
    | Error e ->
      Lwt_io.eprintf "mk_state_stream: %s\n" e >>=
      loop
  in
  Lwt.async loop;
  stream

let state_change_stream stream =
  let prev = ref None in
  Lwt_stream.filter (fun cur ->
      match !prev with
      | None -> prev := Some cur;
        true
      | Some p -> prev := Some cur;
        p <> cur)
    stream

let simple_server port stream =
  let handle client_addr (in_chan, out_chan) =
    Lwt_stream.iter_s
      (fun state ->
         Lwt_io.fprint out_chan
           (if state then "true\n" else "false\n"))
      (state_change_stream stream)
  in
  Lwt_io.establish_server_with_client_address
    (Unix.ADDR_INET (Unix.inet_addr_any, port))
    handle

let _ =
  Lwt_main.run begin
    let stream = mk_state_stream () in
    simple_server 9999 stream
    >>= fun server ->
    Lwt_stream.iter_s (fun state ->
        Lwt_io.printf "Open state: %b\n" state)
      (state_change_stream stream)
  end
