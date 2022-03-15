

let filename = ref ""
let glossary_name = ref ""

let name = ref ""
let start_chapter = ref 1 
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

let msg = "htmlfromtex --input <file> --output <out_file> [--use-glossary <glossary> | --name <name> | --start-chapter <chapter>]";;
let _ =
  Arg.parse spec (fun _ -> ()) msg;
  if !filename = "" || !outname = "" then (print_endline msg; exit 0)
  else if not (String.equal "" !glossary_name) then (Htmlfromtexbooks.Lib.total_glossaries (!glossary_name));
       parse_filename !filename !outname !start_chapter !name
