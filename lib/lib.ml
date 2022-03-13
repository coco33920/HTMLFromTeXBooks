type chapter = Nil | Chap of string * string * string list (*name # list of lines*)
type book = chapter list
let string_to_list s = List.of_seq (String.to_seq s);;
let parse_book ?(book=true) str = 
    let a = if book then Str.regexp "\\chapter" else Str.regexp "\\section" in
    Str.split a str;;


let print_chapter = function
  | Nil -> print_string "Nil\n"
  | Chap(s,_,q) -> Printf.printf "Name : %s\n" s; List.iteri (fun i str -> Printf.printf "Line %d : %s\n" i str) q

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

  
let extract_chapter chapter = 
  let rec extract_name lst acc = 
    match lst with 
      | [] -> acc,""
      | t::q when t='}' -> acc,transform_list_of_chars_to_string_without_the_newline q
      | t::q when t='{' -> extract_name q acc
      | t::q when t='\\' -> extract_name q acc
      | t::q -> extract_name q (acc^(String.make 1 t)) in
  let line_list = List.filter (fun a -> not (String.equal a "\\")) (Str.split (Str.regexp "\n") chapter) in
  match line_list with 
    | [] -> Nil
    | t::q -> let name,r = extract_name (string_to_list t) "" in 
    let q = if r="" then q else r::q in
    let total_str = String.concat "\n" q in
    Chap(name,total_str,q)

let extract_chapters file =
  let str = read_file file in
  let chappies = parse_book str in
  let rec aux acc = function
    | [] -> acc
    | t::q -> aux ((extract_chapter t)::acc) q
  in (aux [] chappies : book);;