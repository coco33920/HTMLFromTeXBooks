open Htmlfromtexbooks.Lib
let s = read_file "/home/charlotte/Documents/dev/htmlfromtexbooks/test.txt";;
let l = parse_book s;;
let chapters = List.mapi (fun i a -> transform_chapter (extract_chapter a i)) l;;
let _ = List.iteri (fun i a -> test_chapter_to_file a ("test"^(string_of_int i)^".html" )) chapters;;
