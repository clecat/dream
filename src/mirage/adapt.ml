(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin

   XXX(dinosaure): same as [src/http/adapt.ml] without [address_to_string] - which
   depends on [Unix]. *)

module Dream = Dream_pure
module Stream = Dream_pure.Stream

(* TODO Write a test simulating client exit during SSE; this was killing the
   server at some point. *)
(* TODO LATER Will also need to monitor buffer accumulation and use flush. *)
(* TODO Rewrite using Dream.next. *)
let forward_body_general
    (response : Dream.response)
    (_write_string : ?off:int -> ?len:int -> string -> unit)
    (write_buffer : ?off:int -> ?len:int -> Dream.buffer -> unit)
    http_flush
    close =
  let bytes_since_flush = ref 0 in

  let rec send () =
    Dream.client_stream response
    |> fun stream ->
      Stream.read
        stream
        ~data
        ~close
        ~flush
        ~ping
        ~pong

  and data chunk off len _binary _fin =
    write_buffer ~off ~len chunk;
    bytes_since_flush := !bytes_since_flush + len;
    if !bytes_since_flush >= 4096 then begin
      bytes_since_flush := 0;
      http_flush send
    end
    else
      send ()

  and flush () =
    bytes_since_flush := 0;
    http_flush send

  and ping _buffer _offset _length =
    send ()

  and pong _buffer _offset _length =
    send ()

  in

  send ()

let forward_body
    (response : Dream.response)
    (body : Dream_httpaf.Body.Writer.t) =

  forward_body_general
    response
    (Dream_httpaf.Body.Writer.write_string body)
    (Dream_httpaf.Body.Writer.write_bigstring body)
    (Dream_httpaf.Body.Writer.flush body)
    (fun _code -> Dream_httpaf.Body.Writer.close body)

let forward_body_h2
    (response : Dream.response)
    (body : [ `write ] H2.Body.t) =

  forward_body_general
    response
    (H2.Body.write_string body)
    (H2.Body.write_bigstring body)
    (H2.Body.flush body)
    (fun _code -> H2.Body.close_writer body)
