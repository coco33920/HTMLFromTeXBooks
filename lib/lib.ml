(**
Library used for the project
@author Charlotte Thomas
*)


(**
Utilities
*)

(** A function reading the inputed file and outputs a string of the lines separated by a line break \n
@param filename the file to read *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let a = input_line chan in if not (String.starts_with ~prefix:"//" a) then lines := a :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

(** Write a string in a file *)
let write_to_file file str =
  let f = open_out file in
  output_string f str;
  flush f;
  close_out f;;


(**
Type declaration and aliases useful
*)


(**Type representing a section*)
type section = Nil (**Nul*)
              | Node of string * string list  (**Node: a node is a string, the text of the node, text list *)
              | Subsubsection of string * section * int
              | Subsection of string * section list * int (**Subsection: a subsection is the subsection of a text*)
              | Section of string * section list * int (**A section is a name and the list of all nodes/subsection*)

(** Chapter type : A chapter is Nil Or A tuple of a string (name), a string (the chapter text) 
a string list (the lines of chapter text) and an integer (the number of the chapter) *)
type chapter = Nill (**Nul*)
              | Section of section list
              | Chap of string * chapter (**A chapter is a name and a section*)

(** Glossary type : It can be a Nil or an "Entry" : a tuple of a string (the key), a string (the name) and a string (the definition) *)
type glossary = Nul | Entry of string * string * string;;

(** Alias : a book is a list of chapters *)
type book = chapter list;;


(*
Parse a book into a list of chapters
Parse each chapters into a list of section
Parse each section into a list of subsection
Parse each subsection into a list of subsubsection
Construct the AST
*)

let string_to_list = fun c -> List.of_seq (String.to_seq c);;
let list_to_string list = String.of_seq (List.to_seq list)

let detect_file_type file =
  let fs = try open_in file |> input_line with End_of_file -> (print_endline "not a valid TeX file"; exit 2)
  in let rec read_line result lst = 
    match lst with 
      | [] -> result
      | t::_ when t='}' -> result
      | t::q -> read_line (result ^ (String.make 1 t)) q
  in let rec read lst =
    match lst with
      | [] -> failwith "Only books/article are supported"
      | t::q when t='{' -> read_line "" q
      | _::q -> read q
  in let type_read = read (string_to_list fs)
  in match type_read with 
    | "book" -> "book"
    | "article" -> "article"
    | _ -> print_endline "only articles/books are supported"; exit 2;;

let extract_name f = 
  let rec read_name result lst = 
    match lst with
      | [] -> result,[]
      | t::q when t='}' -> result,q
      | t::q -> read_name (result^(String.make 1 t)) q
  in let rec read lst = 
    match lst with 
      | [] -> "",""
      | t::q when t='{' -> 
        let name,q = read_name "" q in
        name,(list_to_string q)
      | _::q -> read q
  in read (string_to_list f);;
let create_subsubsection i str =
  if str="" then Nil else
  let newline = Str.regexp "\n" in
  let line_list = Str.split newline str
  in let name,rest = extract_name (List.hd line_list)
  in let new_list = rest::(List.tl line_list)
  in Subsubsection(name,Node((String.concat "\n" new_list),new_list),i);;


let parse_subsubsection str =
  let subsubsection = Str.regexp "\\\\subsubsection" in
  let subsubsection_list = Str.split subsubsection str in
  List.mapi (create_subsubsection) subsubsection_list;;


let create_subsection i str =
  if str = "" then Nil else 
  let newline = Str.regexp "\n"  
  in let line_list = Str.split newline str
  in let name,rest = (extract_name (List.hd line_list))
  in let tl = List.tl line_list
  in let subsubsection_list = parse_subsubsection (String.concat "\n" tl)
  in let first_node = Node(rest, [rest])
  in let subsubsection_list = first_node::subsubsection_list
  in Subsection(name,subsubsection_list,i);;



let parse_subsection str = 
  let subsection_regexp = Str.regexp "\\\\subsection" in
  let subsection_list  = Str.split subsection_regexp str in 
  List.mapi (create_subsection) subsection_list;;

let create_section i str = 
  if str = "" then Nil else
  let newline = Str.regexp "\n"
  in let line_list = Str.split newline str 
  in let name,rest = (extract_name (List.hd line_list));;


(*let create_subsection i lst = 
  if List.length lst = 0 then Nil else
  let hd,tl = List.hd lst,List.tl lst in
  let tl = List.map (parse_subsubsection) tl in


let parse_subsection str = 
  let subsection_regexp = Str.regexp "\\\\subsection" in
  let subsection_list = Str.split subsection_regexp str in
  if List.length subsection_list > 0 then
  let a,q = List.hd subsection_list,List.tl subsection_list
  in let name,rest = extract_name a in let s = List.map parse_subsubsection q
  in 

  else []


let parse_sections str =
  let sections_regexp = Str.regexp "\\\\section" in
  Str.split sections_regexp str |> List.map (parse_subsection)

let parse_chapters str =
  let book_regexp = Str.regexp "\\\\chapter" in
  Str.split book_regexp str |> List.map (parse_sections)*)

let parse_file file =
  let a = detect_file_type file in
  let str = read_file file in
  let str = String.concat "\n" str in
  match a with
    | "book" -> parse_chapters str
    | "chapters" -> [(parse_sections str)]
    | _ -> failwith "only articles books are supported";;