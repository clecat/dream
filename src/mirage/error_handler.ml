module Dream = Dream_pure
module Catch = Dream__middleware.Catch

let log =
  Dream__middleware.Log.sub_log "dream.mirage"

let select_log = function
  | `Error -> log.error
  | `Warning -> log.warning
  | `Info -> log.info
  | `Debug -> log.debug

let dump (error : Catch.error) =
  let buffer = Buffer.create 4096 in
  let p format = Printf.bprintf buffer format in

  begin match error.condition with
  | `Response response ->
    let status = Dream.status response in
    p "%i %s\n" (Dream.status_to_int status) (Dream.status_to_string status)

  | `String "" ->
    p "(Library error without description payload)\n"

  | `String string ->
    p "%s\n" string

  | `Exn exn ->
    let backtrace = Printexc.get_backtrace () in
    p "%s\n" (Printexc.to_string exn);
    backtrace |> Dream__middleware.Log.iter_backtrace (p "%s\n")
  end;

  p "\n";

  let layer =
    match error.layer with
    | `TLS -> "TLS library"
    | `HTTP -> "HTTP library"
    | `HTTP2 -> "HTTP2 library"
    | `WebSocket -> "WebSocket library"
    | `App -> "Application"
  in

  let blame =
    match error.caused_by with
    | `Server -> "Server"
    | `Client -> "Client"
  in

  let severity =
    match error.severity with
    | `Error -> "Error"
    | `Warning -> "Warning"
    | `Info -> "Info"
    | `Debug -> "Debug"
  in

  p "From: %s\n" layer;
  p "Blame: %s\n" blame;
  p "Severity: %s" severity;

  begin match error.client with
  | None -> ()
  | Some client -> p "\n\nClient: %s" client
  end;

  begin match error.request with
  | None -> ()
  | Some request ->
    let last = Dream.last request in

    let major, minor = Dream.version last in
    p "\n\n%s %s HTTP/%i.%i"
      (Dream.method_to_string (Dream.method_ last))
      (Dream.target last)
      major minor;

    Dream.all_headers last
    |> List.iter (fun (name, value) -> p "\n%s: %s" name value);

    let show_variables kind =
      kind (fun name value first ->
        if first then
          p "\n";
        p "\n%s: %s" name value;
        false)
        true
        request
      |> ignore
    in
    show_variables Dream.fold_locals
  end;

  Buffer.contents buffer

let customize template (error : Catch.error) =

  (* First, log the error. *)

  begin match error.condition with
  | `Response _ -> ()
  | `String _ | `Exn _ as condition ->

    let client =
      match error.client with
      | None -> ""
      | Some client ->  " (" ^ client ^ ")"
    in

    let layer =
      match error.layer with
      | `TLS -> ["TLS" ^ client]
      | `HTTP -> ["HTTP" ^ client]
      | `HTTP2 -> ["HTTP/2" ^ client]
      | `WebSocket -> ["WebSocket" ^ client]
      | `App -> []
    in

    let description, backtrace =
      match condition with
      | `String string -> string, ""
      | `Exn exn ->
        let backtrace = Printexc.get_backtrace () in
        Printexc.to_string exn, backtrace
    in

    let message = String.concat ": " (layer @ [description]) in

    select_log error.severity (fun log ->
      log ?request:error.request "%s" message);
    backtrace |> Dream__middleware.Log.iter_backtrace (fun line ->
      select_log error.severity (fun log ->
        log ?request:error.request "%s" line))
  end;

  (* If Dream will not send a response for this error, we are done after
      logging. Otherwise, if debugging is enabled, gather a bunch of information.
      Then, call the template, and return the response. *)

  if not error.will_send_response then
    Lwt.return_none

  else
    let debug_dump = dump error in

    let response =
      match error.condition with
      | `Response response -> response
      | _ ->
        let status =
          match error.caused_by with
          | `Server -> `Internal_Server_Error
          | `Client -> `Bad_Request
        in
        (* TODO Simplify the streams creation. *)
        let client_stream = Dream.Stream.(stream empty no_writer)
        and server_stream = Dream.Stream.(stream no_reader no_writer) in
        Dream.response ~status client_stream server_stream
    in

    (* No need to catch errors when calling the template, because every call
        site of the error handler already has error handlers for catching double
        faults. *)
    response
    |> template error debug_dump
    |> Lwt.map (fun response -> Some response)

let default_response = function
  | `Server ->
    let client_stream = Dream.Stream.(stream empty no_writer)
    and server_stream = Dream.Stream.(stream no_reader no_writer) in
    Dream.response ~status:`Internal_Server_Error client_stream server_stream
  | `Client ->
    let client_stream = Dream.Stream.(stream empty no_writer)
    and server_stream = Dream.Stream.(stream no_reader no_writer) in
    Dream.response ~status:`Bad_Request client_stream server_stream

let default_template _error _debug_dump response =
  Lwt.return response

let debug_template _error debug_dump response =
  let status = Dream.status response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  response
  |> Dream.with_header "Content-Type" Dream_pure.Formats.text_html
  |> Dream.with_body
    (Dream__middleware.Error_template.render ~debug_dump ~code ~reason)
  |> Lwt.return

let default =
  customize default_template

let debug_error_handler =
  customize debug_template

let double_faults f default =
  Lwt.catch f begin fun exn ->
    let backtrace = Printexc.get_backtrace () in
    log.error (fun log -> log "Error handler raised: %s" (Printexc.to_string exn));
    backtrace
    |> Dream__middleware.Log.iter_backtrace (fun line ->
      log.error (fun log -> log "%s" line));
    default ()
  end

let httpaf user's_error_handler = fun client_address ?request:_ error start_response ->
  let condition, severity, caused_by = match error with
    | `Exn exn ->
      `Exn exn,
      `Error,
      `Server
    | `Bad_request
    | `Bad_gateway ->
      `String "Bad request",
      `Warning,
      `Client
    | `Internal_server_error ->
      `String "Content-Length missing or negative",
      `Error,
      `Server in
  let error = {
    Catch.condition;
    layer = `HTTP;
    caused_by;
    request = None;
    response = None;
    client = Some client_address;
    severity;
    will_send_response = true;
  } in

  Lwt.async begin fun () ->
    double_faults begin fun () ->
      let%lwt response = user's_error_handler error in
      let response = match response with
        | Some response -> response
        | None -> default_response caused_by in
      let headers = Httpaf.Headers.of_list (Dream.all_headers response) in
      let body = start_response headers in
      Adapt.forward_body response body;
      Lwt.return_unit
    end
      Lwt.return
  end

let respond_with_option f =
  double_faults
    (fun () ->
      f ()
      |> Lwt.map (function
        | Some response -> response
        | None ->
          (* TODO Simplify streams. *)
          let client_stream = Dream.Stream.(stream empty no_writer)
          and server_stream = Dream.Stream.(stream no_reader no_writer) in
          Dream.response
            ~status:`Internal_Server_Error client_stream server_stream))
    (fun () ->
      (* TODO Simplify streams. *)
      let client_stream = Dream.Stream.(stream empty no_writer)
      and server_stream = Dream.Stream.(stream no_reader no_writer) in
      Dream.response
        ~status:`Internal_Server_Error client_stream server_stream
      |> Lwt.return)

let app user's_error_handler = fun error ->
  respond_with_option (fun () -> user's_error_handler error)
