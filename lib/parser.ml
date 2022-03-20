type cmd = 
  | NullCommand
  | AtomCmd of string
  | SimpleCmd of string * string
  | MultipleCmd of string * string list
type structure =  (*OK*)
  | Nul 
  | Line of string 
  | Cmd of cmd 
  | AtomicCmd of string 
  | OneArgCmd of string * structure list 
  | MultipleArgCmd of string * structure list list
  | Env of string * structure list 
  | Subsubsection of string * structure list
  | Subsection of string * structure list 
  | Section of string * structure list 
  | Chapter of string * structure list 


let parse_to_html ?(min_chap=1) ast= 
  let count = [|1;1;1;1|] in
  let rec aux ?(write=true) acc ast = 
  match ast with
    | [] -> acc
    | Nul::q -> aux acc q
    | Line s::q -> 
      let line= if write then Printf.sprintf "%s\n" s else ""
      in aux ~write:write (acc^line) q
    | AtomicCmd s::q ->
      let new_line = (match s with
        | "par" -> "<br/>\n"
        | "bigskip" -> "</p>\n\n<p>\n"
        | "\\" -> "<br/>\n"
        | "printglossaries" -> ""
        | "item" -> "·"
        | "newline" -> "<br/>\n"
        | _ -> "")
      in let new_acc = if write then acc^new_line^"\n" else ""
      in aux ~write:write new_acc q
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
      in let new_acc = if write then acc^(new_line) else ""
      in aux ~write:write new_acc q
    | Chapter (s,l)::q -> 
      let chapnum = count.(0) in
          begin
            count.(0) <- count.(0) + 1;
            count.(1) <- 1;
            count.(2) <- 1;
            count.(3) <- 1;
          end;
          let str = aux ~write:(chapnum>=min_chap) "" l in
          let new_line = if chapnum>=min_chap then Printf.sprintf "<h1>Chapter %i : %s</h1><br/>\n" chapnum s else "" in
          aux ~write:write (acc^new_line^str) q
    | Section (s,l)::q -> 
      let chapnum,secnum = count.(0),count.(1) in
      begin
        count.(1) <- count.(1) + 1;
        count.(2) <- 1;
        count.(3) <- 1;
      end;
      let str = aux ~write:write "" l in
      let new_line = Printf.sprintf "<h2>Section %i.%i : %s</h2><br/>\n" chapnum secnum s in
      aux ~write:write (acc^new_line^str) q
    | Subsection (s,l)::q -> 
      let chapnum,secnum,ssecnum = count.(0),count.(1),count.(2) in
      begin
        count.(2) <- count.(2) + 1;
        count.(3) <- 1;
      end;
      let str = aux "" l in
      let new_line = Printf.sprintf "<h3>Subsection %i.%i.%i : %s</h3><br/>\n" chapnum secnum ssecnum s in
      aux ~write:write (acc^new_line^str) q
    | Subsubsection (s,l)::q ->
      let chapnum,secnum,ssecnum,sssecnum = count.(0),count.(1),count.(2),count.(3) in
      begin
        count.(3) <- count.(3) + 1;
      end;
      let str = aux ~write:write "" l in
      let new_line = Printf.sprintf "<h4>Subsubsection %i.%i.%i.%i : %s</h4><br/>\n" chapnum secnum ssecnum sssecnum s in
      aux ~write:write (acc^new_line^str) q
    | Env (s,l)::q -> 
      let str = aux ~write:write "" l in 
      let new_line = (match s with
        | "document" -> str
        | "center" -> Printf.sprintf "<div style=\"margin: auto; text-align: center;\">\n%s\n</div>" str
        | _ -> str)
      in aux ~write:write (acc^new_line^"\n") q
    | _::q -> aux acc q
  in aux "" ast;;



let prepare_body name str =
  let line = "<title>" ^ name ^ "</title>\n"
  in let line = line ^ "<body>\n"
  in let line = line ^ "<div style=\"margin: auto; text-align:center;\">\n" 
  in let line = line ^ (Printf.sprintf "<h1>%s</h1>\n" name)
  in let line = line ^ "</div>\n"
  in let line = line ^ str ^ "\n"
  in let line = line ^ "</body>"
  in line;;

(*TODO: prendre en compte les nested cmd pour éviter les }} rémanent*)
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


let print_file_in_html ?(min_chap=1) file name outname =
  let a = pre_parse_file file in
  let a = parse_to_html ~min_chap:min_chap a in 
  prepare_body name a 
  |> Utils.write_to_file outname;;