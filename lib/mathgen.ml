open Parser

let generate_latex_command s l = 
  let line = "\\"^s in
  let args = String.concat "," l in 
  let line = if args="" then line else Printf.sprintf "%s[%s]" line args in
  line;;
  


let generate_latex l = 
  let rec unparse acc l = 
    match l with 
      | [] -> String.concat " " acc 
      | Line s::q -> unparse (s::acc) q 
      | AtomicCmd (s,l)::q -> 
        let line = generate_latex_command s l in 
        unparse (line::acc) q
      | OneArgCmd (s,l,l2)::q ->
        let line = generate_latex_command s l in 
        let line = Printf.sprintf "%s{%s}" line (unparse [] l2) in
        unparse (line::acc) q 
      | MultipleArgCmd (s,l,l2)::q ->
        let line = generate_latex_command s l in
        let l = List.map (unparse []) l2 in  
        let line = Printf.sprintf "%s{%s}" line (String.concat "\n" l) in
        unparse (line::acc) q
      | _::q -> unparse acc q
    in unparse [] l;;

let env_de_latexer env = 
  match env with
    | e -> e;;

let re_calculate_env ast =
  let rec aux acc ast = 
    match ast with
      | [] -> acc
      | Env (s,n)::q when s="align" 
        -> aux (Math(Printf.sprintf "\\begin{align}%s\\end{align}" (generate_latex n))::acc) q 
      | Env (s,n)::q when s="align*" 
        -> aux (Math(Printf.sprintf "\\begin{align*}%s\\end{align*}" (generate_latex n))::acc) q
      | Env (s,n)::q when s="equation" 
        -> aux (Math(Printf.sprintf "\\begin{equation}%s\\end{equation}" (generate_latex n))::acc) q
      | Env (s,n)::q when s="equation*" 
        -> aux (Math(Printf.sprintf "\\begin{equation*}%s\\end{equation*}" (generate_latex n))::acc) q
      | Env(s,n)::q 
        -> let ast = aux [] n in 
           let ast = List.rev ast
           in let env = Env(s,ast)
           in aux (env::acc) q 
      | Chapter (s,l)::q 
        -> let ast = aux [] l in
           let ast = List.rev ast
           in let c = Chapter(s,ast)
           in aux (c::acc) q 
      | Section (s,l)::q 
        -> let ast = aux [] l in
           let ast = List.rev ast
           in let c = Section(s,ast)
           in aux (c::acc) q 
      | Subsection (s,l)::q 
        -> let ast = aux [] l in
           let ast = List.rev ast
           in let c = Subsection(s,ast)
           in aux (c::acc) q 
      | Subsubsection (s,l)::q 
        -> let ast = aux [] l in
           let ast = List.rev ast
           in let c = Subsubsection(s,ast)
           in aux (c::acc) q 
      | e::q -> aux (e::acc) q
  in List.rev (aux [] ast);;