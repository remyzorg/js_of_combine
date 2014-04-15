
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

  let append_endline_to_br elmt str =
    let splited = Regexp.(split (regexp "\n") str) in
    List.iter (fun s ->
      Dom.appendChild elmt (doc##createTextNode (Js.string s));
      Dom.appendChild elmt (Dom_html.createBr doc)
    ) splited

  let read_file f =
    let fin = open_in f in

    let b = Buffer.create 30 in
    let rec step () = Buffer.add_string b (input_line fin); step () in
    begin try step () with End_of_file -> close_in fin end;
    Buffer.contents b

  let print_to_div errbuff outbuff d out_field err_field =
    if Buffer.length outbuff > 0 then begin
      let outp = Dom_html.createP doc in
      append_endline_to_br outp (Buffer.contents outbuff);

      let fs = read_file "out.svg" in
      (* let text_node = doc##createTextNode (Js.string fs) in *)
      let div_svg = Dom_html.createDiv doc in
      (* div_svg##innerHTML <- Js.string fs; *)
      (* let text_node = doc##createElementNS *)
      (*   (Js.string "http://www.w3.org/2000/svg", Js.string "svg") in *)
      div_svg##innerHTML <- Js.string fs;


      (* Dom_html.window##alert (Js.string fs); *)
      (* let svg = Svg.Of_dom.of_element text_node in *)
      Dom.insertBefore (To_dom.of_node out_field) div_svg
        ((To_dom.of_node out_field)##firstChild);
      Dom.insertBefore (To_dom.of_node out_field) outp
        ((To_dom.of_node out_field)##firstChild)

    end;
    if Buffer.length errbuff > 0 then begin
      let errp = Dom_html.createP doc in
      append_endline_to_br errp (Buffer.contents errbuff);
      Dom.insertBefore (To_dom.of_node err_field) errp
        ((To_dom.of_node err_field)##firstChild)
    end;
    Buffer.reset errbuff;
    Buffer.reset outbuff


        (* Dom_html.window##alert (Js.string (Buffer.contents (outbuff))); *)

  let interp errbuff outbuff d out_field err_field _ _ =
    let open Combine in
    let errfmt = Format.formatter_of_buffer errbuff in
    let outfmt = Format.formatter_of_buffer outbuff in
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
    print_to_div errbuff outbuff d out_field err_field;
    Lwt.return ()


}}



{client{
  let create_tarea () =
    let input = Dom_html.createTextarea ~_type:(Js.string "text")
      ~name:(Js.string "test") doc in
    input##setAttribute (Js.string "rows", Js.string "30") ;
    input##setAttribute (Js.string "cols", Js.string "80") ;
    input
}}
