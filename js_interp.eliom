
{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  open Html5.D

}}

{client{


let get_timestamp () =
  let date = jsnew Js.date_now () in
  Js.to_float (date##getTime ())

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

let getContent node =
    let area = Js.Opt.get (node##firstChild) (fun _ -> raise Not_found) in
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


let print_to_div errbuff outbuff d out_field (* err_field *) =
  let outp = Dom_html.createP doc in
  let splited = Regexp.(split (regexp "\n") (Buffer.contents outbuff)) in
  List.iter (fun s ->
    Dom.appendChild outp (doc##createTextNode (Js.string s));
    Dom.appendChild outp (Dom_html.createBr doc)
  ) splited;

  (* let errp = Dom_html.createP doc in *)
  (* let splited = Regexp.(split (regexp "\n") (Buffer.contents errbuff)) in *)
  (* List.iter (fun s -> *)
  (*   Dom.appendChild errp (doc##createTextNode (Js.string s)); *)
  (*   Dom.appendChild errp (Dom_html.createBr doc) *)
  (* ) splited; *)

  (* Dom.appendChild (To_dom.of_node err_field) errp; *)
  Dom.appendChild (To_dom.of_node out_field) outp;
  Buffer.reset errbuff;
  Buffer.reset outbuff


let interp errbuff outbuff d out_field _ _ =
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
        let start, stop = pos in
        Format.fprintf errfmt "File \"%s\", line %d, characters %d-%d : @\n"
          start.pos_fname start.pos_lnum
          (start.pos_cnum - start.pos_bol)
          (stop.pos_cnum - stop.pos_bol) ;
        Format.fprintf errfmt "Error: %a@\n" JsInterp.print_error err;
        Dom_html.window##alert (Js.string (Buffer.contents (outbuff)));
    | e ->
      Format.fprintf errfmt "Uncaught exception: %s@." (Printexc.to_string e)
  end;
  print_to_div errbuff outbuff d out_field;
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
