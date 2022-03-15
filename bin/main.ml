let filename = ref ""
let glossary_name = ref ""

let name = ref ""
let start_chapter = ref (-1) 
let outname = ref ""
let spec = [
  ("--input", Arg.Set_string filename, "specify the input to the program");
  ("--output", Arg.Set_string outname, "specify the ouput to the program");
  ("--name", Arg.Set_string name, "Name of the file");
  ("--start-chapter", Arg.Set_int start_chapter, "Starting chapters");
  ("--use-glossary", Arg.Set_string glossary_name, "specify the glossary")
]

let execute_command file outfile start_chapter name =
  Htmlfromtexbooks.Lib.parse_book (Htmlfromtexbooks.Lib.read_file file) |>
  List.filteri (fun i _ -> (i>=start_chapter)) |>
  List.mapi (fun i a -> Htmlfromtexbooks.Lib.extract_chapter a i) |>
  List.map (Htmlfromtexbooks.Lib.transform_chapter) |>
  Htmlfromtexbooks.Lib.write_book ~name:name outfile;;

let parse_filename file outfile start_chapter name = 
  if not (Sys.file_exists file) then (Printf.printf "The %s file do not exists" file; exit 2)
  else execute_command file outfile start_chapter name;;

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
  in if not (Sys.file_exists b) then Sys.mkdir b 0o755 
  else if not (Sys.is_directory b) then (Sys.remove b; Sys.mkdir b 0o755);
  let c = b ^ (Filename.dir_sep) ^ ".config" in 
  if not (Sys.file_exists c) then (let d = open_out c in write_default_configuration d; close_out d;);
  let f = open_in c 
  in let c = input_line f in let d = input_line f
  in let d,c = (Str.global_replace (Str.regexp "name=") "" d),(Str.global_replace (Str.regexp "start_chapter=") "" c)
  in d,int_of_string c;;


let detect_a_file () = 
  let a = Sys.readdir "."
  |> Array.map (fun a -> if a="glossary.tex" then "" else a) 
  |> Array.map (Filename.extension)
  |> Array.mapi (fun i a -> if String.equal a ".tex" then i else -1) 
  |> Array.find_opt (fun i -> not(i=(-1)))
  in match a with
    | None -> ""
    | Some v -> (Sys.readdir ".").(v);; 

let msg = "htmlfromtex --input <file> --output <out_file> [--use-glossary <glossary> | --name <name> | --start-chapter <chapter>]";;
let _ =
  Arg.parse spec (fun _ -> ()) msg;
  let default_name,default_starting_chapter = load_configuration () in
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
  let chapters = Htmlfromtexbooks.Lib.extract_chapters !filename
  in let tbl = Htmlfromtexbooks.Lib.detect_prelude chapters
  in if List.length chapters = 0 then (print_endline "File is empty"; exit 2)
  else
    if !glossary_name = "" then
      let a = try Hashtbl.find tbl "gloss" with _ -> "" in
      glossary_name := a;
      Printf.printf "Glossary have been extracted from the file and is %s\n" !glossary_name;
  else
    Printf.printf "Glossary filename is %s\n" !glossary_name;
  if !name = "" then
    let n = try Hashtbl.find tbl "title" with _ -> default_name in
    name := n;
    Printf.printf "Name have been automatically extracted as %s\n" !name
  else
    Printf.printf "Name is %s\n" !name;
  if !start_chapter = -1 then
    (start_chapter := default_starting_chapter;
    Printf.printf "Starting chapter have been automatically generated and is %d\n" !start_chapter)
  else
    Printf.printf "Starting chapter is %d\n" !start_chapter;

  Htmlfromtexbooks.Lib.init_glossary !glossary_name;
  parse_filename !filename !outname !start_chapter !name;;