type client
type server

type 'a message

type request = client message
type response = server message

type handler = request -> response Lwt.t
type middleware = handler -> handler

module Make
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (Stack : Mirage_stack.V4V6) : sig

  type route

  type method_ =
    [ `GET
    | `POST
    | `PUT
    | `DELETE
    | `HEAD
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `PATCH
    | `Method of string ]

  type informational =
    [ `Continue
    | `Switching_Protocols ]
  
  type successful =
    [ `OK
    | `Created
    | `Accepted
    | `Non_Authoritative_Information
    | `No_Content
    | `Reset_Content
    | `Partial_Content ]
  
  type redirection =
    [ `Multiple_Choices
    | `Moved_Permanently
    | `Found
    | `See_Other
    | `Not_Modified
    | `Temporary_Redirect
    | `Permanent_Redirect ]
  
  type client_error =
    [ `Bad_Request
    | `Unauthorized
    | `Payment_Required
    | `Forbidden
    | `Not_Found
    | `Method_Not_Allowed
    | `Not_Acceptable
    | `Proxy_Authentication_Required
    | `Request_Timeout
    | `Conflict
    | `Gone
    | `Length_Required
    | `Precondition_Failed
    | `Payload_Too_Large
    | `URI_Too_Long
    | `Unsupported_Media_Type
    | `Range_Not_Satisfiable
    | `Expectation_Failed
    | `Misdirected_Request
    | `Too_Early
    | `Upgrade_Required
    | `Precondition_Required
    | `Too_Many_Requests
    | `Request_Header_Fields_Too_Large
    | `Unavailable_For_Legal_Reasons ]
  
  type server_error =
    [ `Internal_Server_Error
    | `Not_Implemented
    | `Bad_Gateway
    | `Service_Unavailable
    | `Gateway_Timeout
    | `HTTP_Version_Not_Supported ]
  
  type standard_status =
    [ informational
    | successful
    | redirection
    | client_error
    | server_error ]

  type status =
    [ standard_status
    | `Status of int ]

  type 'a local

  val method_to_string : [< method_ ] -> string

  val random : int -> string

  val log : ('a, Format.formatter, unit, unit) format4 -> 'a

  type ('a, 'b) conditional_log =
    ((?request:request ->
     ('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b) ->
      unit

  type log_level = [ `Error | `Warning | `Info | `Debug ]

  val initialize_log :
    ?backtraces:bool ->
    ?async_exception_hook:bool ->
    ?level:[< log_level ] ->
    ?enable:bool ->
      unit -> unit

  val error     : ('a, unit) conditional_log
  val warning   : ('a, unit) conditional_log
  val info      : ('a, unit) conditional_log
  (* val debug     : ('a, unit) conditional_log *)

  val client : request -> string
  val method_ : request -> method_
  val target : request -> string

  val header : string -> 'a message -> string option
  val body : 'a message -> string Lwt.t
  val query : string -> request -> string option
  val queries : string -> request -> string list

  val html : ?status:status -> ?code:int -> ?headers:(string * string) list -> string -> response Lwt.t
  val json : ?status:status -> ?code:int -> ?headers:(string * string) list -> string -> response Lwt.t

  val param : string -> request -> string

  val new_local : ?name:string -> ?show_value:('a -> string) -> unit -> 'a local
  val local : 'a local -> 'b message -> 'a option
  val with_local : 'a local -> 'a -> 'b message -> 'b message

  type csrf_result =
    [ `Ok
    | `Expired of float
    | `Wrong_session
    | `Invalid ]

  val csrf_token : ?valid_for:float -> request -> string
  val verify_csrf_token : request -> string -> csrf_result Lwt.t

  type 'a form_result =
    [ `Ok            of 'a
    | `Expired       of 'a * float
    | `Wrong_session of 'a
    | `Invalid_token of 'a
    | `Missing_token of 'a
    | `Many_tokens   of 'a
    | `Wrong_content_type ]

  type multipart_form =
    (string * ((string option * string) list)) list

  val form : ?csrf:bool -> request -> (string * string) list form_result Lwt.t
  val multipart : ?csrf:bool -> request -> multipart_form form_result Lwt.t

  val form_tag :
    ?method_:method_ ->
    ?target:string ->
    ?enctype:[ `Multipart_form_data ] ->
    ?csrf_token:bool ->
      action:string -> request -> string

  val lowercase_headers : middleware
  val content_length : middleware

  val logger : middleware
  val log : ('a, Format.formatter, unit, unit) format4 -> 'a

  val router : route list -> middleware
  val scope : string -> middleware list -> route list -> route

  val get : string -> handler -> route
  val post    : string -> handler -> route
  val put     : string -> handler -> route
  val delete  : string -> handler -> route
  val head    : string -> handler -> route
  val connect : string -> handler -> route
  val options : string -> handler -> route
  val trace   : string -> handler -> route
  val patch   : string -> handler -> route
    val not_found : handler

  type error =
    { condition :
        [ `Response of response
        | `String of string
        | `Exn of exn ]
    ; layer :
        [ `App
        | `HTTP
        | `HTTP2
        | `TLS
        | `WebSocket ]
    ; caused_by :
        [ `Server
        | `Client ]
    ; request : request option
    ; response : response option
    ; client : string option
    ; severity : log_level
    ; will_send_response : bool }

  type error_handler = error -> response option Lwt.t

  val error_template :
    (error -> string -> response -> response Lwt.t) -> error_handler

  val https :
       ?stop:Lwt_switch.t
    -> port:int
    -> ?prefix:string
    -> Stack.TCP.t
    -> ?cfg:Tls.Config.server
    -> ?error_handler:error_handler
    -> handler
    -> unit Lwt.t

  val http :
       ?stop:Lwt_switch.t
    -> port:int
    -> ?prefix:string
    -> ?protocol:[ `Dream_h2 | `HTTP_1_1 ]
    -> Stack.TCP.t
    -> ?error_handler:error_handler
    -> handler
    -> unit Lwt.t
end
