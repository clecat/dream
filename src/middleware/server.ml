(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Dream = Dream_pure



let client_variable =
  Dream.new_local
    ~name:"dream.client"
    ~show_value:(fun client -> client)
    ()

(* TODO What should be reported when the client address is missing? This is a
   sign of local testing. *)
let client request =
  match Dream.local client_variable request with
  | None -> "127.0.0.1:0"
  | Some client -> client

let with_client client request =
  Dream.with_local client_variable client request



let https_variable =
  Dream.new_local
    ~name:"dream.https"
    ~show_value:string_of_bool
    ()

let https request =
  match Dream.local https_variable request with
  | Some true -> true
  | _ -> false

let with_https https request =
  Dream.with_local https_variable https request



(* TODO Eventually remove Dream.request_from_http as all of its functionality
   is moved here. *)
let request ~client ~method_ ~target ~https ~version ~headers server_stream =
  (* TODO Use pre-allocated streams. *)
  let client_stream = Dream.Stream.(stream no_reader no_writer) in
  Dream.request ~method_ ~target ~version ~headers client_stream server_stream
  |> with_client client
  |> with_https https



let html ?status ?code ?headers body =
  (* TODO The streams. *)
  let client_stream = Dream.Stream.(stream (string body) no_writer)
  and server_stream = Dream.Stream.(stream no_reader no_writer) in
  Dream.response ?status ?code ?headers client_stream server_stream
  |> Dream.with_header "Content-Type" Dream.Formats.text_html
  |> Lwt.return

let json ?status ?code ?headers body =
  (* TODO The streams. *)
  let client_stream = Dream.Stream.(stream (string body) no_writer)
  and server_stream = Dream.Stream.(stream no_reader no_writer) in
  Dream.response ?status ?code ?headers client_stream server_stream
  |> Dream.with_header "Content-Type" Dream.Formats.application_json
  |> Lwt.return
