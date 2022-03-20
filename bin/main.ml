let filename = ref ""
let glossary_name = ref ""

let name = ref ""
let start_chapter = ref (1) 
let chapter = ref (-1)
let outname = ref ""
let spec = [
  ("--input", Arg.Set_string filename, "specify the input to the program");
  ("--output", Arg.Set_string outname, "specify the ouput to the program");
  ("--name", Arg.Set_string name, "Name of the file");
  ("--start-chapter", Arg.Set_int start_chapter, "Starting chapters");
  ("--chapter", Arg.Set_int chapter, "compile only a chapter");
]

let execute_command file name outname start_chapter =
  Htmlfromtexbooks.Parser.print_file_in_html ~min_chap:start_chapter file name outname;;

let parse_filename file name outname start_chapter = 
  if not (Sys.file_exists file) then (Printf.printf "The %s file do not exists" file; exit 2)
  else execute_command file name outname start_chapter;;

let write_default_configuration channel = 
  output_string channel "start_chapter=1\n";
  flush channel;
  output_string channel "name=TeX Generator\n";
  flush channel;
  output_string channel "";
  flush channel;;



let load_configuration () = 
  let a = Unix.getenv "HOME" in
  let b = a ^ (Filename.dir_sep) ^ ".htmlfromtexbooks" 
  in if not (Sys.file_exists b) then Unix.mkdir b 0o640 
  else if not (Sys.is_directory b) then (Sys.remove b; Unix.mkdir b 0o755);
  let c = b ^ (Filename.dir_sep) ^ ".config" in 
  if not (Sys.file_exists c) then (let d = open_out c in write_default_configuration d; close_out d;);
  let f = open_in c 
  in let c = input_line f in let d = input_line f
  in let d,c = (Str.global_replace (Str.regexp "name=") "" d),(Str.global_replace (Str.regexp "start_chapter=") "" c)
  in d,int_of_string c;;


let find_opt (a : 'a -> bool) (arr: 'a array)  = 
  let arr = Array.to_list arr in
  let rec find l = 
    match l with
    | [] -> None 
    | t::_ when a t = true -> Some t
    | _::q -> find q
  in find arr;;

let detect_a_file () = 
  let a = Sys.readdir "."
          |> Array.map (fun a -> if a="glossary.tex" then "" else a) 
          |> Array.map (Filename.extension)
          |> Array.mapi (fun i a -> if String.equal a ".tex" then i else -1) 
          |> find_opt (fun i -> not(i=(-1)))
  in match a with
  | None -> ""
  | Some v -> (Sys.readdir ".").(v);; 

let msg = "htmlfromtex --input <file> --output <out_file> [| --name <name> | --start-chapter <chapter>]";;
let _ =
  Arg.parse spec (fun _ -> ()) msg;
  let default_name,_ = load_configuration () in
  if !filename = "" then 
    let a = detect_a_file () in
    if a = "" then (print_string msg; exit 2) else (filename := a);
    Printf.printf "input filename has been automatically detected to be %s\n" !filename;
  else
    Printf.printf "input filename is %s\n" !filename; 
  if !outname = "" then
    let b = (Filename.remove_extension !filename) in (outname := (b^".html"));
    Printf.printf "output filename has been automatically generated to be %s\n" !outname;
  else
    Printf.printf "output filename is %s\n" !outname;
  if !name = "" then
    name := default_name;
  parse_filename !filename !name !outname !start_chapter