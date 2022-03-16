let glossaries = Hashtbl.create 1;;

(** Alias for Sys.file_exists *)
let glossary_exists glossary = Sys.file_exists glossary;;

(** Returns true if the length of the glossary is greater than 0 and false otherwise *)
let glossary_provided () = not ((Hashtbl.length glossaries) = 0);;

(** returns false if a string is empty or a newline *)
let not_is_empty = function
  | "\n" -> false 
  | "" -> false 
  | s when String.trim s = "" -> false 
  | _ -> true;;

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
  in let a,q = read_desc "" (Utils.string_to_list entry) 
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
    let string = Utils.read_file file in
    let string = String.concat "\n" string in
    let list_of_entries = Str.split glossary_regexp string |> List.filter (not_is_empty)
    |> List.map (Str.global_replace newline_regexp "") in
    list_of_entries;; 

let init_glossary glossary = 
  Hashtbl.clear glossaries;
  parse_glossaries glossary |> List.iter (parse_glossary_entry);;
  
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

  (** takes the char list and returns the (name,description) of the given glossary entry and the remaining of the list *)
let recognize_gls lst = 
  let rec aux acc lst = match lst with
    | [] -> acc,[]
    | t::q when t='}' -> acc,q
    | t::q when t='{' -> aux acc q
    | t::q -> aux (acc^(String.make 1 t)) q
  in let desc,q = aux "" lst
  in (Hashtbl.find glossaries desc),desc,q;;
