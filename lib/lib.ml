type chapter = Nil | Chap of string * string * string list * int;; (*name # list of lines*)

type book = chapter list
let string_to_list s = List.of_seq (String.to_seq s);;
let parse_book ?(book=true) str = 
    let a = if book then Str.regexp "\\\\chapter" else Str.regexp "\\\\section" in
    Str.split a str;;


let print_chapter = function
  | Nil -> print_string "Nil\n"
  | Chap(s,_,q,_) -> Printf.printf "Name : %s\n" s; List.iteri (fun i str -> Printf.printf "Line %d : %s\n" i str) q

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let a = input_line chan in if not (String.starts_with ~prefix:"//" a) then lines := a :: !lines
    done; String.concat "\n" !lines
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev !lines) ;;

let is_a_new_line_unix_or_windows a_char next_char = 
  match Char.code a_char,Char.code next_char with 
    | 13,10 -> (true,0)
    | 13,_ -> (true,1)
    | _,10 -> (true,2)
    | _ -> (false,3);;


let transform_list_of_chars_to_string_without_the_newline lst =
  let rec aux acc l =
    match l with 
      | [] -> acc
      | t::q when t='\n' -> aux acc q (*UNIX*)
      | t::q when t='\r' -> aux acc q (*WINDOWS*)
      | t::q -> print_int (Char.code t); print_newline (); aux (acc ^ (String.make 1 t)) q
  in aux "" lst

  
let extract_chapter chapter i = 
  let rec extract_name lst acc = 
    match lst with 
      | [] -> acc,""
      | t::q when t='}' -> acc,transform_list_of_chars_to_string_without_the_newline q
      | t::q when t='{' -> extract_name q acc
      | t::q when t='\\' -> extract_name q acc
      | t::q -> extract_name q (acc^(String.make 1 t)) in
  let line_list = List.filter (fun a -> let a = String.trim a in not (String.equal a "\\") && not (String.equal a "") && not (String.equal a "\n")) 
                (Str.split (Str.regexp "\n") chapter) in
  match line_list with 
    | [] -> Nil
    | t::q -> let name,r = extract_name (string_to_list t) "" in 
    let q = if r="" then q else r::q in
    let total_str = String.concat "\n" q in
    Chap(name,total_str,q,i)

let extract_chapters file =
  let str = read_file file in
  let chappies = parse_book str in
  let a = ref (-1) in
  let rec aux acc = function
    | [] -> acc
    | t::q -> incr a; aux ((extract_chapter t !a)::acc) q
  in List.rev(aux [] chappies : book);;

  let replace_generalities str = 
    let newline_regexp = Str.regexp "\\\\newline" 
    and par_regexp = Str.regexp "\\\\par"
    and bigskip_regexp = Str.regexp "\\\\bigskip"
    and doublebackslash_regexp = Str.regexp "\\\\\\\\"
    and sep_regexp = Str.regexp "\\\\sep"
    and begin_center_regexp = Str.regexp "\\\\begin{center}"
    and end_center_regexp = Str.regexp "\\\\end{center}"
    and end_document_regexp = Str.regexp "\\\\end{document}" 
  in 
    Str.global_replace newline_regexp "<br/>\n" @@
    Str.global_replace par_regexp "<br/>\n" @@
    Str.global_replace bigskip_regexp "</p>\n\n<p>\n" @@
    Str.global_replace doublebackslash_regexp "<br>\n" @@
    Str.global_replace sep_regexp "<br/><div class=\"center\">\n<p>\n<br/><b>***</b><br/>\n</p>\n</div>" @@
    Str.global_replace begin_center_regexp "<div class=\"center\">\n" @@
    Str.global_replace end_center_regexp "\n</div>" @@
    Str.global_replace end_document_regexp "" @@
    str


let transform_chapter chapter = 
  match chapter with
    | Nil -> Nil
    | Chap(s,_,q,i) -> let g = List.map replace_generalities q in 
                              let c = String.concat "\n" g in 
                              let c = "<div>\n\t" ^ (String.trim c) ^ "\n<div><br/>\n" in
                              Chap(String.trim s,c,g,i);;


  let chapter_to_string chapter = 
    match chapter with 
      | Nil -> ""
      | Chap(s,c,_,i) -> let line = "<h2 id=\"" ^ (string_of_int i) ^ "\"> Chapter " ^ (string_of_int i) ^ " : " ^ s ^ "</h2>\n<br>" in line ^ c;;

  let write_to_file file str =
    let f = open_out file in
    output_string f str;
    flush f;
    close_out f;;

  let test_chapter_to_file chapter file = write_to_file file (chapter_to_string chapter);;