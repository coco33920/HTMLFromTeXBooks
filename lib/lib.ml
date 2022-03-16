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

let list_of_string str = List.of_seq (String.to_seq str)

(**Type representing a section*)
type section = Nil (**Nul*)
              | Node of string * string list  (**Node: a node is a string, the text of the node, text list *)
              | BigNode of section list
              | Subsubsection of string * section list * int
              | Subsection of string * section list * int (**Subsection: a subsection is the subsection of a text*)
              | Section of string * section list * int (**A section is a name and the list of all nodes/subsection*)
              | Chap of string * section  list * int

(** Glossary type : It can be a Nil or an "Entry" : a tuple of a string (the key), a string (the name) and a string (the definition) *)
type glossary = Nul | Entry of string * string * string;;


let put_int_into_node i = function
  | Nil -> Nil
  | Node(a,b) -> Node(a,b)
  | Subsubsection(a,b,_) -> Subsubsection(a,b,i)
  | Subsection(a,b,_) -> Subsection(a,b,i)
  | Section(a,b,_) -> Section(a,b,i)
  | BigNode(a) -> BigNode(a)
  | Chap(a,b,_) -> Chap(a,b,i);;

let rec expand a lst = 
  match a with
    | [] -> lst
    | t::q -> expand q (t::lst);;

let shuffle lst = 
  let rec redistribute i acc lst = match lst with
    | [] -> acc
    | Nil::q -> redistribute i acc q
    | Node(a,b)::q -> redistribute i (Node(a,b)::acc) q
    | Subsubsection(a,b,_)::q -> redistribute (i+1) (Subsubsection(a,b,i)::acc) q
    | Subsection(a,b,_)::q -> redistribute (i+1) (Subsection(a,b,i)::acc) q
    | Section(a,b,_)::q -> redistribute (i+1) (Section(a,b,i)::acc) q
    | Chap(a,b,_)::q -> redistribute (i+1) (Chap(a,b,i)::acc) q
    | BigNode(a)::q -> redistribute (i) (expand (a) acc) q
  in List.rev (redistribute 0 [] lst);;
  
  let transform_list_of_chars_to_string_without_the_newline lst =
    let rec aux acc l =
      match l with 
        | [] -> acc
        | t::q when t='\n' -> aux acc q (*UNIX*)
        | t::q when t='\r' -> aux acc q (*WINDOWS*)
        | t::q -> print_int (Char.code t); print_newline (); aux (acc ^ (String.make 1 t)) q
    in aux "" lst;;

  let apply_all_replace str = 
    let textit_regexp = Str.regexp "\\\\textit"
    and textbf_regexp = Str.regexp "\\\\textbf"
  in
    let str = Str.global_replace textit_regexp "" str in 
    let str = Str.global_replace textbf_regexp "" str in
    str;;  

  let see_if_a_char_is_okay str n ch =
    (Char.equal (String.get str (String.length str - n)) (ch))

  let execute node_list = 
    let stack = Stack.create () in
    let rec aux last_char result lst = 
      match lst with
        | [] -> apply_all_replace result
        | t::q when t='{' && last_char='t' (*\textit*)
            -> Stack.push "</i>" stack; aux t (result^"<i>") q
        | t::q when t='{' && last_char='f' && see_if_a_char_is_okay result 2 'b' (*\textbf*)
            -> Stack.push "</b>" stack; aux t (result^"<b>") q
        | t::q when t='}' 
            -> if Stack.is_empty stack then (aux t (result^"}") q) 
              else let p = Stack.pop stack 
              in aux t (result^p) q
        | t::q -> aux t (result^(String.make 1 t)) q;
    in let analyze str = aux ' ' " " (list_of_string str)
    in let rec analyze_node node_list = match node_list with
        | [] -> []
        | Nil::q -> analyze_node q 
        | Subsection(name,list,i)::q -> Subsection((analyze name),List.rev (analyze_node list),i)::(analyze_node q) 
        | Section(name,list,i)::q -> Section((analyze name),List.rev(analyze_node list),i)::(analyze_node q)
        | Subsubsection(name,list,i)::q -> Subsubsection((analyze name),List.rev(analyze_node list),i)::(analyze_node q)
        | Chap(name,list,i)::q -> Chap((analyze name),List.rev(analyze_node list), i)::(analyze_node q)
        | Node(str,_)::q -> 
          let str = analyze str 
          in let str_l = Str.split (Str.regexp "\n") str
          in Node(str,str_l)::(analyze_node q)
        | BigNode(_)::q -> analyze_node q
    in analyze_node (analyze_node node_list);;


let print_list_of_section section =
  let rec aux result last_index lst = match lst with
    | [] -> result
    | Nil::q -> aux result last_index q
    | Node(s,_)::l -> aux (result^Printf.sprintf "%s\n" s) last_index l
    | BigNode(a)::q -> let r = aux "" "" a in  aux (result^r) last_index q
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
  if not ((String.starts_with ~prefix:"{" f) || String.starts_with ~prefix:"*{" f) then "",f else 
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
  let a = List.map (fun c -> create_subsubsection 0 c) (subsubsection_list) in
  shuffle a;;
  (*in let a = List.filter (fun c -> not (c=Nil)) a in
  List.mapi (put_int_into_node) a;;*)


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
    in if name="" then BigNode(chapter_list) else cons(name,chapter_list,i);;

let parse_generic ?(transform=(fun x -> x)) regexp func str =
  let re = Str.regexp regexp in
  let re_list = Str.split re str in
  let re_list = transform re_list in
  let a = List.map (fun c -> func 0 c) (re_list)
    in shuffle a;;

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