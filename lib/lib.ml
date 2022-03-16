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
              | Subsubsection of string * section list * int
              | Subsection of string * section list * int (**Subsection: a subsection is the subsection of a text*)
              | Section of string * section list * int (**A section is a name and the list of all nodes/subsection*)
              | Chap of string * section  list * int

let put_int_into_node i = function
  | Nil -> Nil
  | Node(a,b) -> Node(a,b)
  | Subsubsection(a,b,_) -> Subsubsection(a,b,i)
  | Subsection(a,b,_) -> Subsection(a,b,i)
  | Section(a,b,_) -> Section(a,b,i)
  | Chap(a,b,_) -> Section(a,b,i);;


let print_list_of_section section =
  let rec aux result last_index lst = match lst with
    | [] -> result
    | Nil::q -> aux result last_index q
    | Node(s,_)::l -> aux (result^Printf.sprintf "%s\n" s) last_index l
    | Subsubsection(s,sec,i)::l -> 
        let results = aux "" last_index sec in 
        let last_index = (string_of_int i) in
        let title = Printf.sprintf "<h4 id=\"%s.%d\">Subsubsection %s.%d : %s</h4><br>\n" last_index i last_index i (s) in
        let total = result^title^results in
        aux total (Printf.sprintf "%s.%d" last_index i) l
    | Subsection(s,sec,i)::l ->
      let results = aux "" last_index sec in 
      let last_index = (string_of_int i) in
      let title = Printf.sprintf "<h3 id=\"%s.%d\">Subsection %s.%d : %s</h3><br>\n" last_index i last_index i (s) in
      let total = result^title^results in
      aux total (Printf.sprintf "%s.%d" last_index i) l
    | Section(s,sec,i)::l ->
      let results = aux "" last_index sec in
      let last_index = (string_of_int i) in
      let title = Printf.sprintf "<h3 id=\"%s.%d\">Section %s.%d : %s</h3><br>\n" last_index i last_index i (s) in
      let total = result^title^results in
      aux total (Printf.sprintf "%s.%d" last_index i) l
    | Chap(s,sec,i)::l ->
      let results = aux "" last_index sec in 
      let last_index = (string_of_int i) in
      let title = Printf.sprintf "<h1 id=\"%s\">Chapter %d : %s </h1><br>\n" last_index i s in
      let total = result^title^results in
      aux total (string_of_int i) l
  in aux "" "" section 
    



(** Chapter type : A chapter is Nil Or A tuple of a string (name), a string (the chapter text) 
a string list (the lines of chapter text) and an integer (the number of the chapter) *)
type chapter = Nill | Chap of string * section list * int

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
  let str = String.trim str in 
  if str="" then Nil else
  let newline = Str.regexp "\n" in
  let line_list = Str.split newline str
  in let name,rest = extract_name (List.hd line_list) in let name,rest = String.trim name,String.trim rest in
  let new_list = rest::(List.tl line_list)
  in let tl = List.map (String.trim) new_list
  in let tl = List.filter (fun a -> not (String.equal "" a)) tl
  in let str = String.concat "\n" tl
  in Subsubsection(name,[Node((str),tl)],i);;


let parse_subsubsection str =
  let subsubsection = Str.regexp "\\\\subsubsection" in
  let subsubsection_list = Str.split subsubsection str in
  let subsubsection_list = List.tl subsubsection_list in
  let a = List.map (fun c -> create_subsubsection 0 c) (subsubsection_list)
  in let a = List.filter (fun c -> not (c=Nil)) a in
  List.mapi (put_int_into_node) a;;


let create_generic func (cons: string * section list * int -> section) i str = 
  let str = String.trim str in
  if str = "" then Nil else 
    let newline = Str.regexp "\n"
    in let line_list = Str.split newline str 
    in let name,rest = extract_name (List.hd line_list) in let name,rest = String.trim name,String.trim rest
    in let tl = List.tl line_list
    in let chapter_list = func (String.concat "\n" tl)
    in let tl = List.map (String.trim) tl
    in let tl = List.filter (fun a -> not (String.equal "" a)) tl
    in let str = String.concat "\n" tl
    in let chapter_list = if chapter_list=[] then [Node(str,[str])] else chapter_list 
    in let first_node = if rest = "" then Nil else Node(rest,[rest])
    in let chapter_list = if rest = "" then chapter_list else first_node::chapter_list
    in cons(name,chapter_list,i);;

let parse_generic ?(transform=(fun x -> x)) regexp func str =
  let re = Str.regexp regexp in
  let re_list = Str.split re str in
  let re_list = transform re_list in
  let a = List.map (fun c -> func 0 c) (re_list)
  in let a = List.filter (fun c -> not (c=Nil)) a in
  List.mapi (put_int_into_node) a;;

let create_subsection = create_generic (parse_subsubsection) (fun (a,b,c) -> Subsection(a,b,c))
let parse_subsection = parse_generic "\\\\subsection" (create_subsection) 
let create_section = create_generic (parse_subsection) (fun (a,b,c) -> Section(a,b,c))
let parse_section = parse_generic "\\\\section" (create_section)
let create_chapter = create_generic (parse_section) (fun (a,b,c) -> Chap(a,b,c))
let parse_chapter = parse_generic ~transform:List.tl "\\\\chapter" (create_chapter)

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

let parse_file file =
  let a = detect_file_type file in
  let str = read_file file in
  let str = String.concat "\n" str in
  let str = replace_generalities str in
  match a with
    | "book" -> parse_chapter str
    | "article" -> parse_chapter str
    | _ -> failwith "only articles books are supported";;