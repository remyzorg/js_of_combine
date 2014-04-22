
{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D

}}

{client{

  let _ =
    Sys_js.register_autoload "combine"
      (fun s -> Some s)

  let get_timestamp () =
    let date = jsnew Js.date_now () in
    (Js.to_float (date##getTime ())) /. 1000.

  module N = struct
    type t = int64
    let zero = Int64.zero
    let one = Int64.one
    let add = Int64.add
    let print fmt n = Format.fprintf fmt "%s" (Int64.to_string n)
  end
  module T = struct
    let gettimeofday = get_timestamp
  end


  module JsInterp = Combine.Interp.Make(T)(N)

  let doc = Dom_html.document

  let raise_nf () = raise Not_found

  let getContent node =
    let area = Js.Opt.get (node##firstChild) raise_nf in
    let area = Js.Unsafe.coerce area in
    area##value

  let parse_string efmt lb =
    let open Lexing in
    let open Combine in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = "current channel" };
    let p =
      Parser.queue Lexer.token lb
    in
    p

  let content_of_string str =
    let splited = Regexp.(split (regexp "\n") str) in
    let with_br = List.fold_left (fun acc line ->
      br () :: pcdata line :: acc
    ) [] splited
    in
    List.rev with_br


  let read_file f =
    let fin = open_in f in
    let b = Buffer.create 30 in
    let rec step () = Buffer.add_string b (input_line fin); step () in
    begin try step () with End_of_file -> close_in fin end;
    Buffer.contents b



    (*   div_svg##innerHTML <- Js.string fs; *)
    (*   (\* Dom_html.window##alert (Js.string fs); *\) *)
    (*   (\* let svg = Svg.Of_dom.of_element text_node in *\) *)
    (*   Dom.insertBefore (To_dom.of_node out_field) div_svg *)
    (*     ((To_dom.of_node out_field)##firstChild); *)
    (*   Dom.insertBefore (To_dom.of_node out_field) outp *)
    (*     ((To_dom.of_node out_field)##firstChild) *)



  let outbuff = Buffer.create 30
  let errbuff = Buffer.create 30
  let errfmt = Format.formatter_of_buffer errbuff
  let outfmt = Format.formatter_of_buffer outbuff

  let append_elmt d elmt =
    Dom.appendChild (To_dom.of_element d)
      (To_dom.of_element elmt)

  let outputs_signal, set_outputs = React.S.create ("", "")

  let reactive_outputs_node : Html5_types.div elt React.signal =
    let react (outs, errs) =
      D.div [
        p ~a:[a_class ["left_element"; "output_div"]] (content_of_string outs);
        p ~a:[a_class ["left_element"; "error_div"]] (content_of_string errs)
      ]
    in
    React.S.l1 react outputs_signal

  let clear_buff_event, set_clear_buff = React.E.create ()
  let react_to_clear_buff () =
    Buffer.reset outbuff; Buffer.reset errbuff
  let () = ignore(React.E.map react_to_clear_buff clear_buff_event)


  let example_input_signal, set_input_example = React.S.create ""

  let reactive_input : Html5_types.textarea elt React.signal =
    let react s =
      D.raw_textarea ~name:"" ~value:s ()
    in
    React.S.l1 react example_input_signal





  let interp d _ _ =
    let open Combine in
    begin
      let to_interp = Js.to_string (getContent (To_dom.of_node d)) in
      let lb = Lexing.from_string to_interp in
      try
        let tree = parse_string errfmt lb in
        JsInterp.interp outfmt errfmt tree
      with
      | Lexer.Lexical_error msg ->
        Format.fprintf errfmt "%a@\nlexical error: %s@."
          Lexer.print_loc lb msg
      | Parser.Error ->
        Format.fprintf errfmt "%a@\nsyntax error@." Lexer.print_loc lb
      | JsInterp.Error (pos, err) ->
        let open Lexing in
        let start, stop = pos in
        Format.fprintf errfmt "File \"%s\", line %d, characters %d-%d : @\n"
          start.pos_fname start.pos_lnum
          (start.pos_cnum - start.pos_bol)
          (stop.pos_cnum - stop.pos_bol) ;
        Format.fprintf errfmt "Error: %a@\n" JsInterp.print_error err
      | e ->
        Format.fprintf errfmt "Uncaught exception: %s@." (Printexc.to_string e)
    end;
    set_outputs (Buffer.contents outbuff, Buffer.contents errbuff);
    Lwt.return ()



  let create_tarea () =
    let tarea = R.node reactive_input in
    let input = To_dom.of_element tarea in
    (* let input = Dom_html.createTextarea ~_type:(Js.string "text") *)
    (*   ~name:(Js.string "test") doc in *)
    input##setAttribute (Js.string "rows", Js.string "30") ;
    input##setAttribute (Js.string "cols", Js.string "80") ;
    input
}}
