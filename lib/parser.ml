type cmd = 
  | NullCommand
  | AtomCmd of string
  | SimpleCmd of string * string
  | MultipleCmd of string * string list
type structure = 
  | Nul (*OK*)
  | Line of string (*OK*)
  | Cmd of cmd  (*OK*)
  | AtomicCmd of string (*OK*)
  | OneArgCmd of string * structure list 
  | MultipleArgCmd of string * structure list list
  | Env of string * structure list (*OK*)
  | Subsubsection of string * structure list
  | Subsection of string * structure list 
  | Section of string * structure list 
  | Chapter of string * structure list 
  | Document of structure
  | Preamble of structure
type ast = 
  | Nil 
  | Node of structure * ast * ast




let parse_to_html ast = 
  let rec aux acc ast = 
  match ast with
    | [] -> acc
    | Nul::q -> aux acc q
    | Line s::q -> 
      let line= Printf.sprintf "%s\n" s
      in aux (acc^line) q
    | AtomicCmd s::q ->
      let new_line = (match s with
        | "par" -> "<br/>\n"
        | "bigskip" -> "</p>\n\n<p>\n"
        | "\\" -> "<br/>\n"
        | "printglossaries" -> ""
        | "item" -> "·"
        | "newline" -> "<br/>\n"
        | _ -> "")
      in aux (acc^new_line^"\n") q
    | OneArgCmd (s,l)::q -> 
      let str = aux "" l in 
      let new_line = (match s with
        | "par" -> "<br/>\n"
        | "bigskip" -> "</p>\n\n<p>\n"
        | "\\" -> "<br/>\n"
        | "printglossaries" -> ""
        | "item" -> "·"
        | "newline" -> "<br/>\n"
        | "textit" -> (Printf.sprintf "<i>%s</i>" str) 
        | "textbf" -> (Printf.sprintf "<b>%s</b>" str)
        | "url" -> (Printf.sprintf "<a href=\"%s\">%s</a>" (Str.global_replace (Str.regexp "\n") "" str) str)
        | _ -> str)
      in aux (acc^new_line) q
    | Chapter (s,l)::q -> 
      let str = aux "" l in
      let new_line = Printf.sprintf "<h1>Chapter %s</h1><br/>\n" s in
      aux (acc^new_line^str) q 
    | Section (s,l)::q -> 
      let str = aux "" l in
      let new_line = Printf.sprintf "<h2>Section %s</h2><br/>\n" s in
      aux (acc^new_line^str) q
    | Subsection (s,l)::q -> 
      let str = aux "" l in
      let new_line = Printf.sprintf "<h3>Subsection %s</h3><br/>\n" s in
      aux (acc^new_line^str) q
    | Subsubsection (s,l)::q -> 
      let str = aux "" l in
      let new_line = Printf.sprintf "<h4>Subsubsection %s : </h1><br/>\n" s in
      aux (acc^new_line^str) q
    | Env (s,l)::q -> 
      let str = aux "" l in 
      let new_line = (match s with
        | "document" -> str
        | "center" -> Printf.sprintf "<div style=\"margin: auto; text-align: center;\">\n%s\n</div>" str
        | _ -> str)
      in aux (acc^new_line^"\n") q
    | _::q -> aux acc q
  in aux "" ast;;
        
(**
Si on rencontre un \
  => On appelle parse_command
*)

(**
Parse_command lit les chars 
  => si il rencontre un { sur une AtomCmd
    => SimpleCmd (String text) (...)
  => Si il rencontre un { sur une SimpleCmd
    => Création d'une liste
  => Si il rencontre un { sur une MultipleCmd ajoute à la commande
  Quand on rencontre le dernier } => on renvoie
*)

(*prendre en compte les nested cmd pour éviter les }} rémanent*)
let rec parse_interior_of_an_accolade list_of_chars acc = 
  match list_of_chars with 
    | [] -> acc,[]
    | t::q when t='}' -> acc,q
    | t::q -> parse_interior_of_an_accolade q (acc^(String.make 1 t));;

let parse_command list_of_chars =
  let rec parse_command_rec list_of_chars acc = 
    match list_of_chars with
      | [] -> acc,[]
      | t::q when t='{' -> 
        let a,q = parse_interior_of_an_accolade q "" in
        let acc = (match acc with 
          | NullCommand -> AtomCmd a 
          | AtomCmd b -> SimpleCmd (b,a) 
          | SimpleCmd (b,c) -> MultipleCmd (b,a::c::[]) 
          | MultipleCmd (b,c) -> MultipleCmd(b,a::c))
        in parse_command_rec q acc
      | t::q when (t='\n' || t=' ') -> acc,q
      | t::q -> match acc with
        | NullCommand -> parse_command_rec q (AtomCmd (String.make 1 t))
        | AtomCmd b -> parse_command_rec q (AtomCmd (b^(String.make 1 t)))
        | _ -> acc,(t::q)
  in let cmd,l = parse_command_rec list_of_chars (NullCommand)
  in 
  match cmd with 
    | MultipleCmd (s,l2) -> MultipleCmd (s,List.rev l2),l
    | e -> e,l;;

let append_line str q =
  match (String.trim (str)) with
    | "" -> q
    | e -> Line(e)::q



let rec parse_string str = 
let rec parse current_acc acc lst = 
      match lst with
        | [] -> append_line current_acc acc
        | t::q when t='\\' -> 
          let cmd,l = parse_command q in 
          parse "" (Cmd(cmd)::(append_line current_acc acc)) l
        | t::q -> parse (current_acc^(String.make 1 t)) acc q
and parse_nested_commands ast_list = 
  match ast_list with
    | [] -> []
    | Line s::q -> (Line s)::(parse_nested_commands q)
    | Cmd c::q -> 
      let a = (match c with
        | NullCommand -> Nul
        | AtomCmd s -> AtomicCmd s
        | SimpleCmd (s,s2) -> OneArgCmd (s,(parse_string s2))
        | MultipleCmd (s,s2) -> MultipleArgCmd (s,(List.map (parse_string) s2)))
      in a::(parse_nested_commands q)
    | _::q -> parse_nested_commands q
  in let a = parse "" [] (Utils.string_to_list str)
  in let a = parse_nested_commands a
  in List.rev a;; 

(*Preamble / Document*)

let separate_preamble lst = 
  let rec iter ast_list a0 a1 =
    match ast_list with
      | (OneArgCmd (s,(Line s1)::_))::q when (String.equal s "begin") && (String.equal s1 "document") -> 
          iter q a0 (OneArgCmd (s,(Line s1)::[])::a1)
      | e::q when (List.length a1 = 0) -> iter q (e::a0) a1 
      | e::q -> iter q a0 (e::a1)
      | [] -> List.rev a0,List.rev a1 
  in iter lst [] [];;
 
let calculate_environments lst =
  let rec extract_env acc lst = 
    match lst with 
      | [] -> acc,[]
      | (OneArgCmd (s,(Line s1)::_))::q when (String.equal s "begin") ->
        let env,l = extract_env [] q in 
        let env = Env (s1,List.rev env) in
        extract_env (env::acc) l
      | (OneArgCmd (s,_))::q when (String.equal s "end") -> acc,q
      | e::q -> extract_env (e::acc) q
  in let a,_ = extract_env [] lst in a;;

let separate_sections lst = 
  let tab = [|false;false;false;false|] in
  let rec extract_section acc lst = 
    match lst with 
      | [] -> acc,[]
      | (OneArgCmd (s,(Line s1)::_))::q when (s="chapter" || s="chapter*")  -> 
        if tab.(0) = true then (tab.(0) <- false; acc,(OneArgCmd (s,(Line s1)::[]))::q)
        else
          let a,l = extract_section [] q in
          let chap = Chapter(s1,List.rev a) in
          tab.(0) <- true;
          extract_section (chap::acc) l
      | (OneArgCmd (s,(Line s1)::_))::q when (s="section" || s="section*")  -> 
        if tab.(1) = true then (tab.(1) <- false; acc,(OneArgCmd (s,(Line s1)::[]))::q)
        else
          let a,l = extract_section [] q in
          let chap = Section(s1,List.rev a) in
          tab.(1) <- true;
          extract_section (chap::acc) l
      | (OneArgCmd (s,(Line s1)::_))::q when (s="subsection" || s="subsection*") -> 
        if tab.(2) = true then (tab.(2) <- false; acc,(OneArgCmd (s,(Line s1)::[]))::q)
        else
          let a,l = extract_section [] q in
          let chap = Subsection(s1,List.rev a) in
          tab.(2) <- true;
          extract_section (chap::acc) l
      | (OneArgCmd (s,(Line s1)::_))::q when (s="subsubsection" || s="subsubsection*")  -> 
        if tab.(3) = true then (tab.(3) <- false; acc,(OneArgCmd (s,(Line s1)::[]))::q)
        else
          let a,l = extract_section [] q in
          let chap = Subsubsection(s1,List.rev a) in
          tab.(3) <- true;
          extract_section (chap::acc) l
      | e::q -> extract_section (e::acc) q
  in let a,_ = extract_section [] lst in List.rev a;;
    
    
       
let pre_parse_file file = 
    let str = Utils.read_file file in
    let str = String.concat "\n" str in
    let a = parse_string str 
    in let  _,doc = separate_preamble a 
    in let doc = separate_sections doc
    in let doc = calculate_environments doc
    in doc;;
