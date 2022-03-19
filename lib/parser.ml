type cmd = 
  | NullCommand
  | AtomCmd of string
  | SimpleCmd of string * string
  | MultipleCmd of string * string list
type structure = 
  | Nul
  | Line of string
  | Cmd of cmd 
  | Env of string * structure list
  | Subsubsection of string * structure
  | Subsection of string * structure
  | Section of string * structure
  | Chapter of string * structure
  | Document of structure
  | Preamble of structure
type ast = 
  | Nil 
  | Node of structure * ast * ast


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


let pre_parse_file file = 
  let str = Utils.read_file file in
  let str = String.concat "\n" str in
  let rec parse current_acc acc lst = 
    match lst with
      | [] -> append_line current_acc acc
      | t::q when t='\\' -> 
        let cmd,l = parse_command q in 
        parse "" (Cmd(cmd)::(append_line current_acc acc)) l
      | t::q -> parse (current_acc^(String.make 1 t)) acc q
  in let a = parse "" [] (Utils.string_to_list str)
  in List.rev a;; 