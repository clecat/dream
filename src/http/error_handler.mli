(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Dream = Dream_pure
module Catch = Dream__middleware.Catch



(* User's error handlers and defaults. These actually generate error response
   templates and/or do logging. *)

val default : Catch.error_handler
val debug_error_handler : Catch.error_handler
val customize :
  (Catch.error -> string -> Dream.response -> Dream.response Lwt.t) ->
    Catch.error_handler



(* Internal functions called by the framework to report errors. These translate
   various libraries' errors into Error.error and call the user's error
   handler. The signatures are arranged so that these helpers can be partially
   applied and then passed in as arguments where the libraries want error
   handler arguments. *)

(* val app :
  Dream.app ->
  Error.error_handler ->
    Dream.middleware *)

val app :
  Catch.error_handler ->
    (Catch.error -> Dream.response Lwt.t)

val httpaf :
  Catch.error_handler ->
    (Unix.sockaddr -> Dream_httpaf.Server_connection.error_handler)

val h2 :
  Catch.error_handler ->
    (Unix.sockaddr -> Dream_h2.Server_connection.error_handler)

val tls :
  Catch.error_handler ->
    (Unix.sockaddr -> exn -> unit)

val websocket :
  Catch.error_handler ->
  Dream.request ->
  Dream.response ->
    (Websocketaf.Wsd.t -> [ `Exn of exn ] -> unit)

val websocket_handshake :
  Catch.error_handler ->
    (Dream.request -> Dream.response -> string -> Dream.response Lwt.t)




(* Logger also used by elsewhere in the HTTP integration. *)
val log : Dream__middleware.Log.sub_log
