(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



module Dream = Dream_pure



let log =
  Log.sub_log "dream.form"

let sort form =
  List.stable_sort (fun (key, _) (key', _) -> String.compare key key') form

type 'a form_result = [
  | `Ok            of 'a
  | `Expired       of 'a * float
  | `Wrong_session of 'a
  | `Invalid_token of 'a
  | `Missing_token of 'a
  | `Many_tokens   of 'a
  | `Wrong_content_type
]

let sort_and_check_form ~now to_value form request =
  let csrf_token, form =
    List.partition (fun (name, _) -> name = Csrf.field_name) form in
  let form = sort form in

  match csrf_token with
  | [_, value] ->
    begin match%lwt Csrf.verify_csrf_token ~now request (to_value value) with
    | `Ok ->
      Lwt.return (`Ok form)

    | `Expired time ->
      Lwt.return (`Expired (form, time))

    | `Wrong_session ->
      Lwt.return (`Wrong_session form)

    | `Invalid ->
      Lwt.return (`Invalid_token form)
    end

  | [] ->
    log.warning (fun log -> log ~request "CSRF token missing");
    Lwt.return (`Missing_token form)

  | _::_::_ ->
    log.warning (fun log -> log ~request "CSRF token duplicated");
    Lwt.return (`Many_tokens form)

let form ?(csrf = true) ~now request =
  match Dream.header "Content-Type" request with
  | Some "application/x-www-form-urlencoded" ->
    let%lwt body = Dream.body request in
    let form = Dream_pure.Formats.from_form_urlencoded body in
    if csrf then
    sort_and_check_form ~now (fun string -> string) form request
    else
    Lwt.return (`Ok (sort form))

  | _ ->
    log.warning (fun log -> log ~request
      "Content-Type not 'application/x-www-form-urlencoded'");
    Lwt.return `Wrong_content_type
