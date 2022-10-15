(**Takes the Parsed AST and transforms it into human-readable and valid HTML files*)

open Parser
open Glossary
open Utils

(**Prints the table of content : all the chapters / section / subsection / subsubsection numeroted*)
let print_table_of_content ast min_chap =
  let count = [| 1; 1; 1; 1 |] in
  let rec aux acc ast =
    match ast with
    | [] -> acc
    | Chapter (s, l) :: q ->
        let chapnum = count.(0) in
        count.(0) <- count.(0) + 1;
        count.(1) <- 1;
        count.(2) <- 1;
        count.(3) <- 1;
        let str = aux "" l in
        let new_line =
          if chapnum >= min_chap then
            Printf.sprintf "<li><a href=\"#c%i\">Chapter %i : %s</a></li>\n"
              chapnum
              (chapnum - min_chap + 1)
              s
          else ""
        in
        aux (acc ^ new_line ^ str) q
    | Section (s, l) :: q ->
        let chapnum, secnum = (count.(0), count.(1)) in
        count.(1) <- count.(1) + 1;
        count.(2) <- 1;
        count.(3) <- 1;
        let str = aux "" l in
        let new_line =
          Printf.sprintf "<li><a href=\"#s%f\">Section %i.%i : %s</a></li>\n"
            ((2. ** float chapnum) *. (3. ** float secnum))
            (chapnum - min_chap + 1)
            secnum s
        in
        aux (acc ^ new_line ^ str) q
    | Subsection (s, l) :: q ->
        let chapnum, secnum, ssecnum = (count.(0), count.(1), count.(2)) in
        count.(2) <- count.(2) + 1;
        count.(3) <- 1;
        let str = aux "" l in
        let new_line =
          Printf.sprintf
            "<li><a href=\"#ss%f\">Subsection %i.%i.%i : %s</a></li>\n"
            ((2. ** float chapnum)
            *. (3. ** float secnum)
            *. (5. ** float ssecnum))
            (chapnum - min_chap + 1)
            secnum ssecnum s
        in
        aux (acc ^ new_line ^ str) q
    | Subsubsection (s, l) :: q ->
        let chapnum, secnum, ssecnum, sssecnum =
          (count.(0), count.(1), count.(2), count.(3))
        in
        count.(3) <- count.(3) + 1;
        let str = aux "" l in
        let new_line =
          Printf.sprintf
            "<li><a href=\"#sss%f\">Subsubsection %i.%i.%i.%i : %s</a></li>\n"
            ((2. ** float chapnum)
            *. (3. ** float secnum)
            *. (5. ** float ssecnum)
            *. (7. ** float sssecnum))
            (chapnum - min_chap + 1)
            secnum ssecnum sssecnum s
        in
        aux (acc ^ new_line ^ str) q
    | Env (_, l) :: q ->
        let a = aux acc l in
        aux (acc ^ a) q
    | _ :: q -> aux acc q
  in
  aux "" ast

(**Parses an AST to HTML, @param min_chap : the first chapter to be written, @param write_before : writing before the first valid chapter*)
let parse_to_html ?(min_chap = 1) write_before ast =
  let count = [| 1; 1; 1; 1 |] in
  let rec aux write acc ast =
    match ast with
    | [] -> acc
    | Nul :: q -> aux write acc q
    | Line s :: q ->
        let line = if write then Printf.sprintf "%s\n" s else "" in
        aux write (acc ^ line) q
    | Math s :: q ->
        let url = Printf.sprintf "https://latex.codecogs.com/svg.image?%s" s in
        let line =
          if write then Printf.sprintf "<img src=\"%s\"/>\n" url else ""
        in
        aux write (acc ^ line) q
    | AtomicCmd (s, _) :: q ->
        let new_line =
          match s with
          | "par" -> "<br/>\n"
          | "bigskip" -> "</p>\n\n<p>\n"
          | "\\" -> "<br/>\n"
          | "printglossaries" -> ""
          | "sep" -> "<div class=\"center\"><b>***</b></div>"
          | "item" -> "·"
          | "newline" -> "<br/>\n"
          | "ast" -> "*"
          | e -> (
              try
                let structure = Hashtbl.find commands e in
                let str = aux write acc structure in
                str
              with _ -> "")
        in
        let new_acc = if write then acc ^ new_line ^ "\n" else "" in
        aux write new_acc q
    | OneArgCmd (s, _, l) :: q ->
        let str = aux write "" l in
        let new_line =
          match s with
          | "par" -> "<br/>\n"
          | "bigskip" -> "</p>\n\n<p>\n"
          | "\\" -> "<br/>\n"
          | "printglossaries" -> ""
          | "item" -> "·"
          | "sep" -> "<div class=\"center\"><b>***</b></div>"
          | "newline" -> "<br/>\n"
          | "ast" -> "*"
          | "gls" -> (
              match l with
              | [] -> ""
              | Line s :: _ ->
                  let name, _ = recognize_gls s in
                  Printf.sprintf "<a href=\"#%s\">%s</a> " s name
              | _ :: _ -> "")
          | "textit" -> Printf.sprintf "<i>%s</i>" str
          | "textbf" -> Printf.sprintf "<b>%s</b>" str
          | "url" ->
              Printf.sprintf "<a href=\"%s\">%s</a>"
                (Str.global_replace (Str.regexp "\n") "" str)
                str
          | e -> (
              try
                let structure = Hashtbl.find commands e in
                let str = aux write acc structure in
                str
              with _ -> "")
        in
        let new_acc = if write then acc ^ new_line else "" in
        aux write new_acc q
    | Chapter (s, l) :: q ->
        let chapnum = count.(0) in
        count.(0) <- count.(0) + 1;
        count.(1) <- 1;
        count.(2) <- 1;
        count.(3) <- 1;
        let write = chapnum >= min_chap in
        let str = aux write "" l in
        let new_line =
          if write then
            Printf.sprintf "<h1 id=\"c%i\">Chapter %i : %s</h1><br/>\n" chapnum
              (chapnum - min_chap + 1)
              s
          else ""
        in
        aux write (acc ^ new_line ^ str) q
    | Section (s, l) :: q ->
        Printf.printf "Section %i\n" count.(0);
        let chapnum, secnum = (count.(0), count.(1)) in
        count.(1) <- count.(1) + 1;
        count.(2) <- 1;
        count.(3) <- 1;
        let str = aux write "" l in
        let new_line =
          Printf.sprintf "<h2 id=\"s%f\">Section %i.%i : %s</h2><br/>\n"
            ((2. ** float chapnum) *. (3. ** float secnum))
            (chapnum - min_chap + 1)
            secnum s
        in
        aux write (acc ^ new_line ^ str) q
    | Subsection (s, l) :: q ->
        let chapnum, secnum, ssecnum = (count.(0), count.(1), count.(2)) in
        count.(2) <- count.(2) + 1;
        count.(3) <- 1;
        let str = aux write "" l in
        let new_line =
          Printf.sprintf "<h3 id=\"ss%f\">Subsection %i.%i.%i : %s</h3><br/>\n"
            ((2. ** float chapnum)
            *. (3. ** float secnum)
            *. (5. ** float ssecnum))
            (chapnum - min_chap + 1)
            secnum ssecnum s
        in
        aux write (acc ^ new_line ^ str) q
    | Subsubsection (s, l) :: q ->
        let chapnum, secnum, ssecnum, sssecnum =
          (count.(0), count.(1), count.(2), count.(3))
        in
        count.(3) <- count.(3) + 1;
        let str = aux write "" l in
        let new_line =
          Printf.sprintf
            "<h4 id=\"sss%f\">Subsubsection %i.%i.%i.%i : %s</h4><br/>\n"
            ((2. ** float chapnum)
            *. (3. ** float secnum)
            *. (5. ** float ssecnum)
            *. (7. ** float sssecnum))
            (chapnum - min_chap + 1)
            secnum ssecnum sssecnum s
        in
        aux write (acc ^ new_line ^ str) q
    | Env (s, l) :: q ->
        let str = aux write "" l in
        let new_line =
          match s with
          | "document" -> str
          | "center" ->
              Printf.sprintf
                "<div style=\"margin: auto; text-align: center;\">\n%s\n</div>"
                str
          | _ -> str
        in
        aux write (acc ^ new_line ^ "\n") q
    | _ :: q -> aux write acc q
  in
  aux write_before "" ast

(**Prepare the body of the HTML file, title, body markups, and adds the style, table of contents and body*)
let prepare_body name str toc =
  let line = "<title>" ^ name ^ "</title>\n" in
  let line = line ^ "<body>\n" in
  let line =
    line ^ "<style>\n.center {\nmargin:auto;\ntext-align:center;\n}\n </style>"
  in
  let line = line ^ "<div class=\"center\">\n" in
  let line = line ^ Printf.sprintf "<h1>%s</h1>\n" name in
  let line = line ^ "<h2>Table of Content</h2>\n" in
  let line = line ^ "<ul>\n" in
  let line = line ^ toc ^ "\n" in
  let line = line ^ "</ul>\n" in
  let line = line ^ "</div>\n" in
  let line = line ^ str ^ "\n" in
  let line = line ^ prints_glossary () ^ "\n" in
  let line = line ^ "</body>" in
  line

(**Performs the first transformation : from reading the file to rendering it html printing ready*)
let pre_parse_file file =
  let str = read_file file in
  let str = String.concat "\n" str in
  let a = parse_string str in
  let p, doc = separate_preamble a in
  read_preamble p;
  let doc = separate_sections doc in
  let doc = calculate_environments doc in
  let doc = Mathgen.re_calculate_env doc in
  (match Hashtbl.find_opt preamble "glossary" with
  | Some s -> init_glossary s
  | None -> ());
  doc

(**Writes the HTML translation of the given TeX file in the outfile. @param min_chap is the first chapter to be writen, @param 
   write_before determines if the parser writes what comes before the first valid chapter*)
let print_file_in_html ?(min_chap = 1) ?(write_before = false) file outname =
  let a = pre_parse_file file in
  let html = parse_to_html ~min_chap write_before a in
  let toc = print_table_of_content a min_chap in
  let name = try Hashtbl.find preamble "title" with _ -> "Generic" in
  prepare_body name html toc |> write_to_file outname
