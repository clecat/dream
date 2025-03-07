(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Dream = Dream_pure



type error = {
  condition : [
    | `Response of Dream.response
    | `String of string
    | `Exn of exn
  ];
  layer : [
    | `App
    | `HTTP
    | `HTTP2
    | `TLS
    | `WebSocket
  ];
  caused_by : [
    | `Server
    | `Client
  ];
  request : Dream.request option;
  response : Dream.response option;
  client : string option;
  severity : Log.log_level;
  will_send_response : bool;
}

type error_handler = error -> Dream.response option Dream.promise

(* This error handler actually *is* a middleware, but it is just one pathway for
   reaching the centralized error handler provided by the user, so it is built
   into the framework. *)

(* TODO The option return value thing is pretty awkward. *)
let catch error_handler next_handler request =

  Lwt.try_bind

    (fun () ->
      next_handler request)

    (fun response ->
      let status = Dream.status response in

      if Dream.is_client_error status || Dream.is_server_error status then begin
        let caused_by, severity =
          if Dream.is_client_error status then
            `Client, `Warning
          else
            `Server, `Error
        in

        let error = {
          condition = `Response response;
          layer = `App;
          caused_by;
          request = Some request;
          response = Some response;
          client = Some (Server.client request);
          severity = severity;
          will_send_response = true;
        } in

        error_handler error
      end
      else
        Lwt.return response)

    (* This exception handler is partially redundant, in that the HTTP-level
       handlers will also catch exceptions. However, this handler is able to
       capture more relevant context. We leave the HTTP-level handlers for truly
       severe protocol-level errors and integration mistakes. *)
    (fun exn ->
      let error = {
        condition = `Exn exn;
        layer = `App;
        caused_by = `Server;
        request = Some request;
        response = None;
        client = Some (Server.client request);
        severity = `Error;
        will_send_response = true;
      } in

      error_handler error)
