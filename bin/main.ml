let filename = ref ""
let glossary_name = ref ""

let name = ref ""
let start_chapter = ref (1) 
let chapter = ref (-1)
let writing = ref false
let outname = ref ""
let spec = [
  ("--input", Arg.Set_string filename, "specify the input to the program");
  ("--output", Arg.Set_string outname, "specify the ouput to the program");
  ("--name", Arg.Set_string name, "Name of the file");
  ("--start-chapter", Arg.Set_int start_chapter, "Starting chapters");
  ("--chapter", Arg.Set_int chapter, "compile only a chapter");
  ("--writing", Arg.Set writing, "writing before the first chapter")
]

let execute_command file outname start_chapter writing =
  Htmlfromtexbooks.Htmlgen.print_file_in_html ~min_chap:start_chapter ~write_before:writing file outname;;

let parse_filename file outname start_chapter writing = 
  if not (Sys.file_exists file) then (Printf.printf "The %s file do not exists" file; exit 2)
  else execute_command file outname start_chapter writing;;

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
  parse_filename !filename !outname !start_chapter !writing;;