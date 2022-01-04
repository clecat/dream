module Dream = Dream_pure

open Rresult
open Lwt.Infix

let to_dream_method meth = Httpaf.Method.to_string meth |> Dream.string_to_method
let to_httpaf_status status = Dream.status_to_int status |> Httpaf.Status.of_code
let to_h2_status status = Dream.status_to_int status |> H2.Status.of_code
let sha1 str = Digestif.SHA1.(to_raw_string (digest_string str))
let const x = fun _ -> x
let ( >>? ) = Lwt_result.bind

let wrap_handler_httpaf _user's_error_handler user's_dream_handler =
  let httpaf_request_handler = fun client reqd ->
    let httpaf_request = Httpaf.Reqd.request reqd in
    let method_ = to_dream_method httpaf_request.meth in
    let target  = httpaf_request.target in
    let version = (httpaf_request.version.major, httpaf_request.version.minor) in
    let headers = Httpaf.Headers.to_list httpaf_request.headers in
    let body    = Httpaf.Reqd.request_body reqd in

    let read ~data ~close ~flush:_ ~ping:_ ~pong:_ =
      Httpaf.Body.Reader.schedule_read
        body
        ~on_eof:(fun () -> close 1000)
        ~on_read:(fun buffer ~off ~len -> data buffer off len true false)
    in
    let close _close =
      Httpaf.Body.Reader.close body in
    let body =
      Dream_pure.Stream.reader ~read ~close in

    let client_stream = Dream.Stream.(stream no_reader no_writer) in
    let server_stream = Dream.Stream.(stream body no_writer) in

    let request = Dream.request ~method_ ~target ~version ~headers client_stream server_stream in

    (* Call the user's handler. If it raises an exception or returns a promise
       that rejects with an exception, pass the exception up to Httpaf. This
       will cause it to call its (low-level) error handler with variand `Exn _.
       A well-behaved Dream app should catch all of its own exceptions and
       rejections in one of its top-level middlewares.

       We don't try to log exceptions here because the behavior is not
       customizable here. The handler itself is customizable (to catch all)
       exceptions, and the error callback that gets leaked exceptions is also
       customizable. *)
    Lwt.async begin fun () ->
      Lwt.catch begin fun () ->
        (* Do the big call. *)
        let%lwt response = user's_dream_handler request in

        (* Extract the Dream response's headers. *)

        (* This is the default function that translates the Dream response to an
           http/af response and sends it. We pre-define the function, however,
           because it is called from two places:

           1. Upon a normal response, the function is called unconditionally.
           2. Upon failure to establish a WebSocket, the function is called to
              transmit the resulting error response. *)
        let forward_response response =
          let headers =
            Httpaf.Headers.of_list (Dream.all_headers response) in

          (* let version =
            match Dream.version_override response with
            | None -> None
            | Some (major, minor) -> Some Httpaf.Version.{major; minor}
          in *)
          let status =
            to_httpaf_status (Dream.status response) in
          (* let reason =
            Dream.reason_override response in *)

          let httpaf_response =
            Httpaf.Response.create ~headers status in
          let body =
            Httpaf.Reqd.respond_with_streaming reqd httpaf_response in

          Adapt.forward_body response body;

          Lwt.return_unit
        in

        forward_response response
      end
      @@ fun exn ->
        (* TODO LATER There was something in the fork changelogs about not
           requiring report_exn. Is it relevant to this? *)
        Httpaf.Reqd.report_exn reqd exn;
        Lwt.return_unit
    end
  in

  httpaf_request_handler

let request_handler
  : Dream__middleware.Catch.error_handler -> Dream.handler -> string -> Alpn.reqd -> unit
  = fun (user's_error_handler : Dream__middleware.Catch.error_handler)
      (user's_dream_handler : Dream.handler) -> ();
    fun client_address -> function
    | Alpn.Reqd_HTTP_1_1 reqd -> wrap_handler_httpaf user's_error_handler user's_dream_handler client_address reqd
    | _ -> assert false

let error_handler
  : Dream__middleware.Catch.error_handler -> string -> ?request:Alpn.request -> Alpn.server_error ->
    (Alpn.headers -> Alpn.body) -> unit
  = fun (user's_error_handler : Dream__middleware.Catch.error_handler) -> ();
    fun client ?request error start_response ->
  match request with
  | Some (Alpn.Request_HTTP_1_1 request) ->
    let start_response hdrs : Httpaf.Body.Writer.t = match start_response Alpn.(Headers_HTTP_1_1 hdrs) with
      | Alpn.Body_HTTP_1_1 (Alpn.Wr, Alpn.Body_wr body) -> body
      | _ -> Fmt.failwith "Impossible to respond with an h2 respond to an HTTP/1.1 client" in
    Error_handler.httpaf user's_error_handler client ?request:(Some request) error start_response
  | _ -> assert false (* TODO *)

module Make (Pclock : Mirage_clock.PCLOCK) (Time : Mirage_time.S) (Stack : Mirage_stack.V4V6) = struct
  include Dream_pure.Stream
  include Dream_pure

  include Dream__middleware.Log
  include Dream__middleware.Log.Make (Pclock)

  let () = initialize ~setup_outputs:ignore

  include Dream__middleware.Echo
  
  let default_log =
    Dream__middleware.Log.sub_log (Logs.Src.name Logs.default)
  
  let error = default_log.error
  let warning = default_log.warning
  let info = default_log.info
  let debug = default_log.debug
  
  include Dream__middleware.Router
  
  include Dream__middleware.Session
  include Dream__middleware.Session.Make (Pclock)

  include Dream__middleware.Origin_referrer_check
  include Dream__middleware.Form
  include Dream__middleware.Upload
  include Dream__middleware.Csrf
  
  let content_length =
    Dream__middleware.Content_length.content_length
  
  include Dream__middleware.Lowercase_headers
  include Dream__middleware.Catch
  include Dream__middleware.Site_prefix
  include Dream__middleware.Query

  let error_template =
    Error_handler.customize

  let random =
    Dream__cipher.Random.random

  include Dream_pure.Formats

  let log =
    Dream__middleware.Log.convenience_log

  include Dream__middleware.Tag

  let respond ?status ?code ?headers body =
    let client_stream = stream (string body) no_writer
    and server_stream = stream no_reader no_writer in
    response ?status ?code ?headers client_stream server_stream
    |> Lwt.return

  let not_found _ =
    respond ~status:`Not_Found ""

  let now () = Ptime.to_float_s (Ptime.v (Pclock.now_d_ps ()))

  let form = form ~now
  let multipart = multipart ~now
  let csrf_token = csrf_token ~now
  let verify_csrf_token = verify_csrf_token ~now
  let form_tag = form_tag ~now

  let client =
    Dream__middleware.Server.client
  let with_client =
    Dream__middleware.Server.with_client
  let https =
    Dream__middleware.Server.https
  let html =
    Dream__middleware.Server.html
  let json =
    Dream__middleware.Server.json

  include Dream_pure.Formats

  include Paf_mirage.Make (Time) (Stack)

  let alpn =
    let module R = (val Mimic.repr tls_protocol) in
    let alpn (_, flow) = match TLS.epoch flow with
      | Ok { Tls.Core.alpn_protocol; _ } -> alpn_protocol
      | Error _ -> None in
    let peer ((ipaddr, port), _) = Fmt.str "%a:%d" Ipaddr.pp ipaddr port in
    let injection (_, flow) = R.T flow in
    { Alpn.alpn; peer; injection; }

  let built_in_middleware prefix error_handler =
    Dream_pure.pipeline [
      Dream__middleware.Lowercase_headers.lowercase_headers;
      Dream__middleware.Content_length.content_length;
      Dream__middleware.Catch.catch (Error_handler.app error_handler);
      Dream__middleware.Site_prefix.with_site_prefix prefix;
    ]

  let localhost_certificate =
    let crts = Rresult.R.failwith_error_msg
      (X509.Certificate.decode_pem_multiple (Cstruct.of_string Dream__localhost.certificate)) in
    let key = Rresult.R.failwith_error_msg
      (X509.Private_key.decode_pem (Cstruct.of_string Dream__localhost.key)) in
    `Single (crts, key)

  let https ?stop ~port ?(prefix= "") stack
    ?(cfg= Tls.Config.server ~certificates:localhost_certificate ())
    ?error_handler:(user's_error_handler : error_handler = Error_handler.default) (user's_dream_handler : handler) =
    let accept t = accept t >>? fun flow ->
      let edn = Stack.TCP.dst flow in
      TLS.server_of_flow cfg flow >>= function
      | Ok flow -> Lwt.return_ok (edn, flow)
      | Error err -> Lwt.return (R.error_msgf "%a" TLS.pp_write_error err) in
    let user's_dream_handler =
      built_in_middleware prefix user's_error_handler user's_dream_handler in
    let error_handler = error_handler user's_error_handler in
    let request_handler =
      request_handler user's_error_handler user's_dream_handler in
    let service = Alpn.service alpn ~error_handler ~request_handler accept close in
    init ~port stack >>= fun t ->
    let `Initialized th = serve ?stop service t in th

  let alpn protocol =
    let protocol = match protocol with
      | `H2 -> "h2"
      | `HTTP_1_1 -> "http/1.1" in
    let module R = (val Mimic.repr tcp_protocol) in
    let alpn _ = Some protocol in
    let peer ((ipaddr, port), _) = Fmt.str "%a:%d" Ipaddr.pp ipaddr port in
    let injection (_, flow) = R.T flow in
    { Alpn.alpn; peer; injection; }

  let http ?stop ~port ?(prefix= "") ?(protocol= `HTTP_1_1) stack
    ?error_handler:(user's_error_handler= Error_handler.default)
    user's_dream_handler =
    let accept t = accept t >>? fun flow ->
      let edn = Stack.TCP.dst flow in
      Lwt.return_ok (edn, flow) in
    let user's_dream_handler =
      built_in_middleware prefix user's_error_handler user's_dream_handler in
    let error_handler = error_handler user's_error_handler in
    let request_handler = request_handler user's_error_handler user's_dream_handler in
    let service = Alpn.service (alpn protocol) ~error_handler ~request_handler accept close in
    init ~port stack >>= fun t ->
    let `Initialized th = serve ?stop service t in th
end

include Dream
