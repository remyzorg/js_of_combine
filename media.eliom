
{shared{
  open Eliom_lib
  open Eliom_content
  open Html5

}}


{client{

}}

let main_service =
  Eliom_registration.Html5.register_service
    ~path:["rel"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        D.(html
           (head (title (pcdata "Media")) [])
           (body [h1 [pcdata "Media"];

                  video
                    ~a:[a_autoplay `Autoplay;a_controls `Controls;a_loop `Loop]
                    ~src:(make_uri (Eliom_service.static_dir ())
                                ["vid.webm"]) []


                 ])))
