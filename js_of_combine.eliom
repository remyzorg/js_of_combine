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


let interp_button s1 d out_field err_field =
  let button = string_input  ~input_type:`Submit () ~value:"Interpret" in
  let _ = {unit{
    Lwt_js_events.(
      async (
        let outbuff = Buffer.create 30 in
        let errbuff = Buffer.create 30 in
        fun () ->
          clicks (To_dom.of_element %button)
            (Js_interp.interp errbuff outbuff %d %out_field %err_field)
          ))
  }}
  in
  button

let div_with_area () =
  let d = div [] in
  let _ = {unit{
    Lwt_js_events.(
      async (fun () ->
        let tarea = Js_interp.create_tarea () in
        Dom.appendChild (Html5.To_dom.of_element %d) tarea;
        Lwt.return ()
      )) }}
  in d
(* ; css_link ~uri:css_uri () *)
let () =
  Js_of_combine_app.register
    ~service:main_service
    (fun name () ->
      Lwt.return begin
        let d = div_with_area () in
        let out_field = div ~a:[a_class ["left_element"; "output_div"]] [p []] in
        let err_field = div ~a:[a_class ["left_element"; "error_div"]] [p []] in
        Eliom_tools.D.html
          ~title:"js_of_combine"
          ~css:[["css";"js_of_combine.css"];
                ["css"; "bootstrap.css"]]





          (body [
            div ~a:[a_class ["navbar"; "navbar-inverse"; "navbar-fixed-top"]][
              div ~a:[a_class ["navbar-inner"]] [
                div ~a:[a_class ["container"]] [
                  a ~a:[a_class ["brand"]]
                    ~service:Eliom_service.void_coservice'
                    ~fragment:"" [pcdata "Combine"] ();
                  div ~a:[a_class ["nav-collapse collapse"]] [

                  (* <ul ~a:[a_class ["nav"]]> *)
                  (*   <li ~a:[a_class ["active"]]><a href="#">Home</a></li> *)
                  (*   <li><a href="#about">About</a></li> *)
                  (*   <li><a href="#contact">Contact</a></li> *)
                  (* </ul> *)
                  ]]]];

            h2 [pcdata "Combine"];
            div [interp_button "Button" d out_field err_field];
            d;
            div ~a:[a_class ["inside"]] [out_field; err_field]
          ])
      end)
