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


let mywidget s1 d out_field =
  let button  = string_input ~input_type:`Submit () ~value:"Interpret" in
  let _ = {unit{
    Lwt_js_events.(
      async (
        let outbuff = Buffer.create 30 in
        let errbuff = Buffer.create 30 in
        fun () ->
          clicks (To_dom.of_element %button)
            (Js_interp.interp errbuff outbuff %d %out_field)
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
let () =
  Js_of_combine_app.register
    ~service:main_service
    (fun name () ->
      Lwt.return (
        let d = div_with_area () in
        let out_field = div (* ~a:[a_class ["inside"]] *) [] in
        (* let err_field = div (\* ~a:[a_class ["inside"]] *\) [] in *)
        Eliom_tools.D.html
          ~title:"js_of_combine"
          ~css:[["css";"js_of_combine.css"]]
          (body [
            h2 [pcdata "Combine"];
            div [mywidget "Button" d out_field];
            d;
            div [out_field]
          ])))
