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
              Lwt.return (set_outputs ("", ""));
          )))
  }}
  in button



let div_with_area () =
  let d = div ~a:[a_class ["left_element"; "console"]] [] in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        let tarea = Js_interp.create_tarea () in
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



(* let sendfile = *)
(*   Eliom_registration.File.register_service *)
(*     ~path:["examples/scott.cmb"] *)
(*     ~get_params:Eliom_parameter.unit *)
(*     (fun () () -> Lwt.return "examples") *)



let file_node dir f =
  let fsrv =
    Eliom_registration.File.register_service
    ~path:[dir ^ f]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return f)
  in
  let a_button = Raw.a [pcdata f] in
  let _ = {unit{
    Lwt_js_events.(
      async (
        fun () ->
          clicks (To_dom.of_element %a_button) (
            fun _ _ -> Lwt.return ();)))
  }}
  in a_button







(* ; css_link ~uri:css_uri () *)
let () =
  Js_of_combine_app.register
    ~service:main_service
    (fun name () ->
      Lwt.return begin
        let outputs_div = div ~a:[a_class ["left_element"; "outputs"]] [] in
        let _ = {unit{
          Js_interp.(append_elmt %outputs_div
                       (R.node reactive_outputs_node))}} in
        let input_div = div_with_area () in

        Eliom_tools.D.html
          ~title:"js_of_combine"
          ~css:[["css";"js_of_combine.css"];
                ["css"; "bootstrap.css"]]
          (body [
            h2 [pcdata "Combine"];
            div [interp_button input_div];
            (* let open Eliom_content.Html5.F in *)
            (* a (Eliom_service.static_dir ()) *)
            (*   [pcdata "download image"] ["scott.cmb"]; *)

            div [input_div;
                 div ~a:[a_class ["left_element"; "files"]]
                   (List.map (fun s -> div [file_node ex_dir s])
                      (file_list  ("static/" ^ ex_dir)));
                 clear_btn ();
                 outputs_div
                ]
          ])

      end)
