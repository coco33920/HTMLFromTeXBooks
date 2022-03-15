(**
Library used for the project
@author Charlotte Thomas
*)


(**
Type declaration and aliases useful
*)


(** Chapter type : A chapter is Nil Or A tuple of a string (name), a string (the chapter text) 
a string list (the lines of chapter text) and an integer (the number of the chapter) *)
type chapter = Nil | Chap of string * string * string list * int;;

(** Glossary type : It can be a Nil or an "Entry" : a tuple of a string (the key), a string (the name) and a string (the definition) *)
type glossary = Nul | Entry of string * string * string;;

(** Alias : a book is a list of chapters *)
type book = chapter list;;


(** Utility *)


(**Transform a string to a list of character
@param s the string to transform *)
let string_to_list s = List.of_seq (String.to_seq s);;

(** The Hashtbl with the (name,defition) of glossaries by their key *)
let glossaries = ((Hashtbl.create 1):(string,string * string) Hashtbl.t);;

(** A little function to print a chapter *)
let print_chapter = function
  | Nil -> print_string "Nil\n"
  | Chap(s,_,q,_) -> Printf.printf "Name : %s\n" s; List.iteri (fun i str -> Printf.printf "Line %d : %s\n" i str) q;;

(** A function reading the inputed file and outputs a string of the lines separated by a line break \n
@param filename the file to read *)
let read_file ?(joining="\n") filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let a = input_line chan in if not (String.starts_with ~prefix:"//" a) then lines := a :: !lines
    done; String.concat joining !lines
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev !lines) ;;

(** Alias for Sys.file_exists *)
let glossary_exists glossary = Sys.file_exists glossary;;

(** Returns true if the length of the glossary is greater than 0 and false otherwise *)
let glossary_provided () = not ((Hashtbl.length glossaries) = 0);;

(** Write a string in a file *)
let write_to_file file str =
  let f = open_out file in
  output_string f str;
  flush f;
  close_out f;;


(** Book functions *)


(** Parse a string into a list of string representing chapters (book) or section (article) *)
let parse_book ?(book=true) str = 
    let a = if book then Str.regexp "\\\\chapter" else Str.regexp "\\\\section" in
    Str.split a str;;

(** Returns a string from a char list, ignoring the lines break *)
let transform_list_of_chars_to_string_without_the_newline lst =
  let rec aux acc l =
    match l with 
      | [] -> acc
      | t::q when t='\n' -> aux acc q (*UNIX*)
      | t::q when t='\r' -> aux acc q (*WINDOWS*)
      | t::q -> print_int (Char.code t); print_newline (); aux (acc ^ (String.make 1 t)) q
  in aux "" lst;;

(** Creating a chapter object from a string and the number of the chapter *)
let extract_chapter chapter i = 
  let rec extract_name lst acc = 
    match lst with 
      | [] -> acc,""
      | t::q when t='}' -> acc,transform_list_of_chars_to_string_without_the_newline q
      | t::q when t='*' -> extract_name q acc
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
    Chap(name,total_str,q,i);;

(** Reads a file, Parses the book and returns a list of chapters with the extract_chapter function *)
let extract_chapters file =
  let str = read_file file in
  let chappies = parse_book str in
  let a = ref (-1) in
  let rec aux acc = function
    | [] -> acc
    | t::q -> incr a; aux ((extract_chapter t !a)::acc) q
  in List.rev(aux [] chappies : book);;

(** takes the char list and returns the (name,description) of the given glossary entry and the remaining of the list *)
let recognize_gls lst = 
  let rec aux acc lst = match lst with
    | [] -> acc,[]
    | t::q when t='}' -> acc,q
    | t::q when t='{' -> aux acc q
    | t::q -> aux (acc^(String.make 1 t)) q
  in let desc,q = aux "" lst
  in (Hashtbl.find glossaries desc),desc,q;;

(** Replaces the common found structure in a TeX file *)
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
  str;;

(** Parse more complex command : \textit, \textbf and \gls *)
let parse_more_than_generalities str =
  let textit_regexp = Str.regexp "\\\\textit"
  and textbf_regexp = Str.regexp "\\\\textbf"
  and gls_regexp = Str.regexp "\\\\gls" in
  let stack = Stack.create () in
    let rec aux last_char result lst = 
      match lst with
        | [] -> result
        | t::q when t='{' && last_char='t' (*\textit*)
            -> Stack.push "</i>" stack; aux t (result^"<i>") q
        | t::q when t='{' && last_char='f' (*\textbf*)
            -> Stack.push "</b>" stack; aux t (result^"<b>") q
        | t::q when t='{' && last_char='s' (*\gls*)
            -> if glossary_provided () then (let  (name,_),(desc),l = (recognize_gls q) in aux '}' (result^"<a href=\""^desc^"\">"^name^"</a>") l)
              else aux t (result^"{") q
        | t::q when t='}' 
            -> if Stack.is_empty stack then (aux t (result^"}") q) 
              else let p = Stack.pop stack in aux t (result^p) q
        | t::q -> aux t (result^(String.make 1 t)) q
  in 
  let s = aux ' ' "" (string_to_list str) in
  let s = Str.global_replace textit_regexp "" s in
  let s = Str.global_replace textbf_regexp ""s in
  let s = if glossary_provided () then let s = Str.global_replace gls_regexp "" s in s else s in
  s;;

(** Transform a chapter with the functions replace_generalities and replace_more_than_generalities *)
let transform_chapter chapter = 
  match chapter with
    | Nil -> Nil
    | Chap(s,_,q,i) -> let g = List.map replace_generalities q in
                                let c = String.concat "\n" g in 
                                let c = parse_more_than_generalities c in
                                let c = "<div>\n\t" ^ (String.trim c) ^ "\n<div><br/>\n" in
                                Chap(String.trim s,c,g,i);;

(** prints a chapter *)
let chapter_to_string chapter = 
  match chapter with 
    | Nil -> ""
    | Chap(s,c,_,i) -> let line = "<h2 id=\"" ^ (string_of_int i) ^ "\"> Chapter " ^ (string_of_int i) ^ " : " ^ s ^ "</h2>\n<br>" in line ^ c;;

(** returns false if a string is empty or a newline *)
  let not_is_empty = function
    | "\n" -> false 
    | "" -> false 
    | s when String.trim s = "" -> false 
    | _ -> true;;


(** Glossary functions *)


(** Reads an entry, extract the description, name and definition, and adds it to the Hashtbl *)
let parse_glossary_entry entry = 
  let name_regexp = Str.regexp "name="
  and desc_regexp = Str.regexp "description=" 
  and accolade_fermante_regexp = Str.regexp "}" 
  and accolade_ouvrante_regexp = Str.regexp "{" in
    let rec read_desc acc = function
      | [] -> acc,[]
      | t::q when t='{' -> read_desc acc q
      | t::q when t='}' -> acc,q
      | t::q -> read_desc (acc^(String.make 1 t)) q 
    and read_name acc = function
      | [] -> acc,[]
      | t::q when t='{' -> read_name acc q
      | t::q when t='}' -> acc,q
      | t::q when t=',' -> acc,q
      | t::q -> read_name (acc^(String.make 1 t)) q
    and read_def acc = function
      | [] -> acc,[]
      | t::q when t='{' -> read_def acc q 
      | t::_ when t='}' -> acc,[]
      | t::q -> read_def (acc^(String.make 1 t)) q
    and s = fun (s) -> String.sub s 1 (String.length s -1)
  in let not_comma_then_s = fun t -> t |> s |> String.trim
  in let a,q = read_desc "" (string_to_list entry) 
  in let b,q2 = read_name "" q
  in let c,_ = read_def "" q2
  in let a = [a;b;c]
  in let a = List.map (fun c -> Str.global_replace accolade_ouvrante_regexp "" (Str.global_replace accolade_fermante_regexp "" c)) a 
  in match a with
    | a::b::c::[] -> let a,b,c = (a,Str.global_replace name_regexp "" b, Str.global_replace desc_regexp "" c) in
                    Hashtbl.add glossaries a ((not_comma_then_s b), (not_comma_then_s c))
    | _ -> ();;

(** Takes a glossary file and parse it in a list of string *)
let parse_glossaries file = 
  let glossary_regexp = Str.regexp "\\\\newglossaryentry" in
  let newline_regexp = Str.regexp "\n" in
  if not (glossary_exists file) then [] 
  else
    let string = read_file file in
    let list_of_entries = Str.split glossary_regexp string |> List.filter (not_is_empty)
    |> List.map (Str.global_replace newline_regexp "") in
    list_of_entries;; 

(** Combines parse_glosaries and parse_glossary_entry *)
let total_glossaries file = 
  parse_glossaries file 
  |> List.map parse_glossary_entry 
  |> ignore;;

(** Transform the glossary to an HTML file *)
let prints_glossary () =
  let line = "<div id=\"glossary\"><br>\n>" in
  let line = String.cat line "<h2>Glossary</h2>" in
  let rec aux result (entries) = 
    match entries with
      | [] -> result
      | (desc,(name,def))::q -> let l = "\t<div id=\"" ^ desc ^ "\">\n\t\t<h3>" ^ name ^ "</h3>\n\t\t<p>" ^ def ^ "</p>\n\t</div>\n"
                                  in aux (String.cat result l) q
  in let line = aux line (List.of_seq (Hashtbl.to_seq glossaries))
  in String.cat line "</div>\n";; 


(** Writing book function *)


(** Generate the table of content HTML *)
let print_table_of_content chapters =
    let line = "<div id=\"toc\" class=\"center\"><br>\n" in
    let line = String.cat line "<h2> Table of Content </h2>" in 
    let rec aux result = function
      | [] -> result
      | Nil::q -> aux result q
      | Chap(name,_,_,i)::q -> let l = Printf.sprintf "<a href=\"#%i\">Chapter %i : %s</a>\n<br>" i i name in 
                                aux (String.cat result l) q
    in let a = aux line chapters 
    in let a = String.cat a (Printf.sprintf "<a href=\"#glossary\">Glossary</a>") in
    String.cat a "</div>";;

(** Prepare the body for the content *)
let prepare_body ?(name="TeX Created File") chapter str =
  let t = "<title>"^name^"</title>" in
  let t = String.cat t "<body>\n" in
  let t = String.cat t "<style>\n .center { \n margin:auto; \n text-align: center; \n } \n </style>" in
  let t = String.cat t "<h1 id=\"title\" class=\"center\">"^name^"</h1>\n\n" in
  let t = String.cat t (print_table_of_content chapter) in 
  let t = String.cat t str in
  let t = String.cat t (prints_glossary ()) in
  let t = String.cat t "</body>" in
  t;;

(** Write single chapter in an HML file *)
  let write_core_chapter_to_file chapter file =
    chapter_to_string chapter
    |> Str.global_replace (Str.regexp "\\\\printglossaries") ""
    |> prepare_body [chapter]
    |> write_to_file file;;
  
(** Write the book in an HTML file *)
let write_book ?(name="TeX Created File") file (chapters: chapter list)= 
  List.map (chapter_to_string) chapters |>
  String.concat "\n\n<br>" |>
  Str.global_replace (Str.regexp "\\\\printglossaries") "" |>
  prepare_body ~name:name chapters |> 
  write_to_file file;;