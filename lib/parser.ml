
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


let print_list_of_section ?(start_chapter=1) section =
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
    | Chap(s,sec,i)::l when i>=start_chapter->
      let results = aux "" last_index sec in 
      let last_index = (string_of_int i) in
      let title = Printf.sprintf "<h1 id=\"%s\">Chapter %d : %s </h1><br>\n" last_index i s in
      let total = result^title^results in
      aux total (string_of_int i) l
    | _::l -> aux result last_index l
  in aux "" "" section;;
    
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
        name,(Utils.list_to_string q)
      | _::q -> read q
  in read (Utils.string_to_list f);;

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
