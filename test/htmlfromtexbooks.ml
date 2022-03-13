open Htmlfromtexbooks.Lib
let s = read_file "/home/charlotte/Documents/dev/htmlfromtexbooks/test.txt";;
let l = parse_book s;;
let _ = List.iter (fun c -> print_chapter (extract_chapter c)) l