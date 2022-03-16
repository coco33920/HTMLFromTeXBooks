open Parser
(**
Library used for the project
@author Charlotte Thomas
*)

(**
Type declaration and aliases useful
*)

let list_of_string str = List.of_seq (String.to_seq str)
  let apply_all_replace str = 
    let textit_regexp = Str.regexp "\\\\textit"
    and textbf_regexp = Str.regexp "\\\\textbf"
    and url_regexp = Str.regexp "\\\\url" 
    and href_regexp = Str.regexp "\\\\href"
    and gls_regexp = Str.regexp "\\\\gls"
  in
    let str = Str.global_replace textit_regexp "" str in 
    let str = Str.global_replace textbf_regexp "" str in
    let str = Str.global_replace url_regexp "" str in
    let str = Str.global_replace href_regexp "" str in
    let str = Str.global_replace gls_regexp "" str in
    str;;  

  let see_if_a_char_is_okay str n ch =
    (Char.equal (String.get str (String.length str - n)) (ch))

  let rec parse_urls acc = function
  | [] -> acc,[]
  | t::q when t='}' -> acc,q
  | t::q -> parse_urls (acc^(String.make 1 t)) q

  let rec parse_href (acc1,acc2) r = function
  | [] -> (acc1,acc2),[]
  | t::q when t='{' -> parse_href (acc1,acc2) r q
  | t::q when t='}' && acc1="" -> parse_href (r,"") "" q
  | t::q when t='}' && acc1<>"" -> (acc1,r),q
  | t::q -> parse_href (acc1,acc2) (r^(String.make 1 t)) q

  let execute node_list = 
    let stack = Stack.create () in
    let rec aux last_char result lst = 
      match lst with
        | [] -> apply_all_replace result
        | t::q when t='{' && last_char='t' (*\textit*)
            -> Stack.push "</i>" stack; aux t (result^"<i>") q
        | t::q when t='{' && last_char='f' && see_if_a_char_is_okay result 2 'b' (*\textbf*)
            -> Stack.push "</b>" stack; aux t (result^"<b>") q
        | t::q when t='{' && last_char = 'f' && see_if_a_char_is_okay result 2 'e' (*\href*)
            -> let (url,name),q = parse_href ("","") "" q in
              let a = Printf.sprintf "<a href=\"%s\">%s</a>" url name in aux t (result^a) q
        | t::q when t='{' && last_char='l' (*\url*)
            -> let url,q = parse_urls "" q 
                in let a = Printf.sprintf "<a href=\"%s\">%s</a>" url url in aux t (result^a) q
        | t::q when t='{' && last_char='s' (*\gls*)
            -> if Glossary.glossary_provided () then 
            let (name,_),desc,l = Glossary.recognize_gls q
            in let a = Printf.sprintf "<a href=\"%s\">%s</a>" desc name
            in aux '{' (result^a) l else aux '{' (result^"}") q
        | t::q when t='}' 
            -> if Stack.is_empty stack then (aux t (result^"}") q) 
              else let p = Stack.pop stack 
              in aux t (result^p) q
        | t::q -> aux t (result^(String.make 1 t)) q;
    in let analyze str = aux ' ' " " (list_of_string str)
    in let rec analyze_node node_list = match node_list with
        | [] -> []
        | Nil::q -> analyze_node q 
        | Subsection(name,list,i)::q -> Subsection((analyze name),List.rev (analyze_node list),i)::(analyze_node q) 
        | Section(name,list,i)::q -> Section((analyze name),List.rev(analyze_node list),i)::(analyze_node q)
        | Subsubsection(name,list,i)::q -> Subsubsection((analyze name),List.rev(analyze_node list),i)::(analyze_node q)
        | Chap(name,list,i)::q -> Chap((analyze name),List.rev(analyze_node list), i)::(analyze_node q)
        | Node(str,_)::q -> 
          let str = analyze str 
          in let str_l = Str.split (Str.regexp "\n") str
          in Node(str,str_l)::(analyze_node q)
        | BigNode(_)::q -> analyze_node q
    in analyze_node (analyze_node node_list);;


let replace_generalities str = 
  let newline_regexp = Str.regexp "\\\\newline" 
  and par_regexp = Str.regexp "\\\\par"
  and bigskip_regexp = Str.regexp "\\\\bigskip"
  and doublebackslash_regexp = Str.regexp "\\\\\\\\"
  and sep_regexp = Str.regexp "\\\\sep"
  and begin_center_regexp = Str.regexp "\\\\begin{center}"
  and end_center_regexp = Str.regexp "\\\\end{center}"
  and end_document_regexp = Str.regexp "\\\\end{document}" 
  and begin_itemize_regexp = Str.regexp "\\\\begin{itemize}"
  and end_itemize_regexp = Str.regexp "\\\\end{itemize}"
  and item_regexp = Str.regexp "\\\\item"
  in 
  Str.global_replace newline_regexp "<br/>\n" @@
  Str.global_replace par_regexp "<br/>\n" @@
  Str.global_replace bigskip_regexp "</p>\n\n<p>\n" @@
  Str.global_replace doublebackslash_regexp "<br>\n" @@
  Str.global_replace sep_regexp "<br/><div class=\"center\">\n<p>\n<br/><b>***</b><br/>\n</p>\n</div>" @@
  Str.global_replace begin_center_regexp "<div class=\"center\">\n" @@
  Str.global_replace end_center_regexp "\n</div>" @@
  Str.global_replace end_document_regexp "" @@
  Str.global_replace begin_itemize_regexp "<br>\n" @@
  Str.global_replace end_itemize_regexp "" @@ 
  Str.global_replace item_regexp "  " @@
  str;;

let parse_file file =
  let a = Utils.detect_file_type file in
  let str = Utils.read_file file in
  let str = String.concat "\n" str in
  let str = replace_generalities str in
  match a with
    | "book" -> parse_chapter str
    | "article" -> parse_chapter str
    | _ -> failwith "only articles books are supported";;
let prepare_body ?(name="TeX Created File") str =
  let t = "<title>"^name^"</title>" in
  let t = String.cat t "<body>\n" in
  let t = String.cat t "<style>\n .center { \n margin:auto; \n text-align: center; \n } \n </style>" in
  let t = String.cat t "<h1 id=\"title\" class=\"center\">"^name^"</h1>\n\n" in
  (*let t = String.cat t (print_table_of_content chapter) in *)
  let t = String.cat t str in
  let t = String.cat t (Glossary.prints_glossary ()) in
  let t = String.cat t "</body>" in
  t;;

let print_file ?(start_chapter=1) filename outfile name = 
  let file = parse_file filename in
  let file = execute file in
  let c = print_list_of_section ~start_chapter:start_chapter file in
  let c = prepare_body ~name:name c in
  Utils.write_to_file outfile c;;