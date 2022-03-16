(**
   Utilities
*)

let string_starts_with prefix str =
  if String.length prefix > String.length str then false else 
    let a = String.sub str 0 (String.length prefix) in String.equal a prefix;;

(** A function reading the inputed file and outputs a string of the lines separated by a line break \n
    @param filename the file to read *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let a = input_line chan in if not (string_starts_with "//" a) then lines := a :: !lines
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


let transform_list_of_chars_to_string_without_the_newline lst =
  let rec aux acc l =
    match l with 
    | [] -> acc
    | t::q when t='\n' -> aux acc q (*UNIX*)
    | t::q when t='\r' -> aux acc q (*WINDOWS*)
    | t::q -> print_int (Char.code t); print_newline (); aux (acc ^ (String.make 1 t)) q
  in aux "" lst;;

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
