{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D

}}

module Js_of_combine_app =
  Eliom_registration.App (
    struct
      let application_name = "js_of_combine"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let ex_dir = "examples/"


let interp_button input_div =
  let button = string_input  ~input_type:`Submit () ~value:"Interpret" in
  let _ = {unit{
    Lwt_js_events.(
      async (
        fun () ->
          clicks (To_dom.of_element %button)
            (Js_interp.interp %input_div)
          ))
  }}
  in
  button


let clear_btn () =
  let button = string_input ~input_type:`Submit () ~value:"Clear" in
  let _ = {unit{
    Lwt_js_events.(
      async (
        fun () ->
          clicks (To_dom.of_element %button) (
            fun _ _ ->
              let open Js_interp in
              set_clear_buff ();
              Lwt.return (set_outputs ("", "", []));
          )))
  }}
  in button



let div_with_area () =
  let d = div ~a:[a_class ["left_element";]] [] in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        let tarea = To_dom.of_element (R.node Js_interp.reactive_input) in
        Dom.appendChild (Html5.To_dom.of_element %d) tarea;
        Lwt.return ()
      )) }}
  in d


let file_list dir =
  let open Unix in
  let dh = opendir dir in
  let lr = ref [] in
  let rec add_step () =
    let f = readdir dh in
    if f <> "." && f <> ".." then lr := f :: !lr;
    add_step ()
  in
  begin try add_step () with End_of_file -> () end;
  !lr


let read_file f =
  let ic = open_in f in
  let b = Buffer.create 30 in
  begin try
    while true do Buffer.add_string b (input_line ic ^ "\n") done
  with End_of_file -> () end;
  Lwt.return (
  Buffer.contents b)

(* type t = (int * string)  deriving(Json) *)

{client{
  let read_server_file = %(server_function Json.t<string> read_file)
}}

let file_node dir f =
  let a_button = Raw.a [pcdata f] in
  let _ = {unit{
    Lwt_js_events.(
      async (
        fun () ->
          clicks (To_dom.of_element %a_button) (
            fun _ _ ->
              lwt s = read_server_file (%dir ^ %f) in
              Js_interp.set_input_example s;
              Lwt.return ();)))
  }}
  in a_button


(* ; css_link ~uri:css_uri () *)
let () =
  Js_of_combine_app.register
    ~service:main_service
    (fun name () ->
      Lwt.return begin
        let outputs_div = div ~a:[a_class ["left_element"; "outputs"]] [] in
        let svgs_div = div [] in
        let _ = {unit{
          Js_interp.(append_elmt %outputs_div
                       (R.node reactive_outputs_node))}} in
        let _ = {unit{
          Js_interp.(append_elmt %svgs_div
                       (R.node reactive_svg_gallery))}} in
        let input_div = div_with_area () in
        Eliom_tools.D.html
          ~title:"js_of_combine"
          ~css:[["css";"js_of_combine.css"];
                ["css"; "bootstrap.css"]]
          (body [
            h2 [pcdata "Combine"];
            div [interp_button input_div];
            div [input_div;
                 div ~a:[a_class ["left_element"; "files"]]
                   (List.map (fun s -> div [file_node ("static/" ^ ex_dir) s])
                      (file_list  ("static/" ^ ex_dir)));
                 clear_btn ();
                 outputs_div
                ];
            svgs_div
          ])

      end)
