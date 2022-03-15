open Htmlfromtexbooks.Lib
let s = read_file "/home/charlotte/Documents/dev/htmlfromtexbooks/agh.tex";;
let l = parse_book s;;



let _ = total_glossaries "glossary.tex";;
let chapters = List.mapi (fun i a -> transform_chapter (extract_chapter a i)) l;;
let _ = List.iteri (fun i a -> write_core_chapter_to_file a ("test"^(string_of_int i)^".html" )) chapters;;
