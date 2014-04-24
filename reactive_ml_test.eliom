
{shared{
  open Eliom_lib
  open Eliom_content
  open Html5

}}


let main_service =
  Eliom_registration.Html5.register_service
    ~path:["rel"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        D.(html
           (head (title (pcdata "Reactive_Eliom")) [])
           (body [h1 [pcdata "Reactive_Eliom"]])))
