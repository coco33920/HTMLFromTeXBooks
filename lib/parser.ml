(**Main module for parsing string to an AST representing LaTeX Books*)

open Utils

(**Commande type : an internal used in the first phase of the parsing*)
type cmd =
  | NullCommand  (**A nullcommand is nothing*)
  | AtomCmd of string * string list
      (**An atomic command just have a name and eventual parameters like \newline*)
  | SimpleCmd of string * string list * string
      (**A command with extactly one argument e.g. \chapter{name}*)
  | MultipleCmd of string * string list * string list
      (**A command with multiple arguments e.g. \frac{x}{3}*)

(**Structure used to represent in an AST-ish (just one node with a list of structure children) LaTeX*)
type structure =
  | Nul
  | Line of string  (**A line of just plain text like "It's the Enterprise!"*)
  | Cmd of cmd  (**A command of our internal type used in the parsing*)
  | AtomicCmd of string * string list
      (**The final type to represent an atomic command like \newline*)
  | OneArgCmd of string * string list * structure list
      (**Structure version of SimpleCdm*)
  | MultipleArgCmd of string * string list * structure list list
      (**Structure version of MultipleCmd*)
  | Env of string * structure list
      (**An environment like document,center,equation...*)
  | Math of string
      (**A re-latexified math literal to be sent to an online image processing*)
  | Subsubsection of string * structure list
      (**Represent the LaTeX subsubsection, a subsubsection have a name and a list of structure children*)
  | Subsection of string * structure list  (**See above*)
  | Section of string * structure list  (**See above*)
  | Chapter of string * structure list  (**See above*)

let preamble = Hashtbl.create 1
let commands = Hashtbl.create 1

(**Parses an accolade for the first time (command reading)*)
let parse_interior_of_an_accolade list_of_chars acc =
  let stack = Stack.create () in
  let rec parse list_of_chars acc =
    match list_of_chars with
    | [] -> (acc, [])
    | t :: q when t = '{' ->
        Stack.push t stack;
        parse q (acc ^ String.make 1 t)
    | t :: q when t = '}' ->
        if Stack.is_empty stack then (acc, q)
        else (
          Stack.pop stack |> ignore;
          parse q (acc ^ String.make 1 t))
    | t :: q -> parse q (acc ^ String.make 1 t)
  in
  parse list_of_chars acc

(**Parses arguments of a function [colorlinks,12pt] to a list of string ["colorlinks","12pt"]*)
let rec parse_arguments list_of_chars current_acc acc =
  match list_of_chars with
  | [] -> (current_acc :: acc, [])
  | t :: q when t = ']' -> (current_acc :: acc, q)
  | t :: q when t = ',' -> parse_arguments q "" (current_acc :: acc)
  | t :: q when t = ' ' -> parse_arguments q current_acc acc
  | t :: q -> parse_arguments q (current_acc ^ String.make 1 t) acc

(**Parses a command recursively, called when a \ is detected in the parsing of a string*)
let parse_command list_of_chars =
  let rec parse_command_rec list_of_chars acc =
    match list_of_chars with
    | [] -> (acc, [])
    | t :: q when t = '{' ->
        let a, q = parse_interior_of_an_accolade q "" in
        let acc =
          match acc with
          | NullCommand -> AtomCmd (a, [])
          | AtomCmd (b, c) -> SimpleCmd (b, c, a)
          | SimpleCmd (b, e, c) -> MultipleCmd (b, e, [ a; c ])
          | MultipleCmd (b, e, c) -> MultipleCmd (b, e, a :: c)
        in
        parse_command_rec q acc
    | t :: q when t = '[' ->
        let a, q = parse_arguments q "" [] in
        let acc =
          match acc with
          | NullCommand -> NullCommand
          | AtomCmd (c, b) -> AtomCmd (c, b @ a)
          | SimpleCmd (b, e, c) -> SimpleCmd (b, e @ a, c)
          | MultipleCmd (b, e, c) -> MultipleCmd (b, e @ a, c)
        in
        parse_command_rec q acc
    | t :: q when t = '\n' || t = ' ' -> (acc, q)
    | t :: q when t = '$' -> (acc, q)
    | t :: q -> (
        match acc with
        | NullCommand -> parse_command_rec q (AtomCmd (String.make 1 t, []))
        | AtomCmd (b, _) ->
            parse_command_rec q (AtomCmd (b ^ String.make 1 t, []))
        | _ -> (acc, t :: q))
  in
  let cmd, l = parse_command_rec list_of_chars NullCommand in
  match cmd with
  | MultipleCmd (s, e, l2) -> (MultipleCmd (s, e, List.rev l2), l)
  | e -> (e, l)

(**Appends a line to a list if the line is not empty*)
let append_line str q = match String.trim str with "" -> q | e -> Line e :: q

(**Parses inline $$ math*)
let parse_math l =
  let rec parse acc l =
    match l with
    | [] -> (acc, [])
    | t :: q when t = '$' -> (acc, q)
    | t :: q -> parse (acc ^ String.make 1 t) q
  in
  let a, b = parse "" l in
  (Math a, b)

(**Parses recursively a string into a list of structure, transforms commands to their final type*)
let rec parse_string str =
  let rec parse current_acc acc lst =
    match lst with
    | [] -> append_line current_acc acc
    | t :: q when t = '$' ->
        let a, q2 = parse_math q in
        parse "" (a :: append_line current_acc acc) q2
    | t :: q when t = '\\' ->
        let cmd, l = parse_command q in
        parse "" (Cmd cmd :: append_line current_acc acc) l
    | t :: q -> parse (current_acc ^ String.make 1 t) acc q
  and parse_nested_commands ast_list =
    match ast_list with
    | [] -> []
    | Line s :: q -> Line s :: parse_nested_commands q
    | Math e :: q -> Math e :: parse_nested_commands q
    | Cmd c :: q ->
        let a =
          match c with
          | NullCommand -> Nul
          | AtomCmd (s, e) -> AtomicCmd (s, e)
          | SimpleCmd (s, e, s2) -> OneArgCmd (s, e, parse_string s2)
          | MultipleCmd (s, e, s2) ->
              MultipleArgCmd (s, e, List.map parse_string s2)
        in
        a :: parse_nested_commands q
    | _ :: q -> parse_nested_commands q
  in
  let a = parse "" [] (string_to_list str) in
  let a = parse_nested_commands a in
  List.rev a

(**Take the list of structure and gives the preamble (everything before the start of the document) and the document itself*)
let separate_preamble lst =
  let rec iter ast_list a0 a1 =
    match ast_list with
    | OneArgCmd (s, e, Line s1 :: _) :: q
      when String.equal s "begin" && String.equal s1 "document" ->
        iter q a0 (OneArgCmd (s, e, [ Line s1 ]) :: a1)
    | e :: q when List.length a1 = 0 -> iter q (e :: a0) a1
    | e :: q -> iter q a0 (e :: a1)
    | [] -> (List.rev a0, List.rev a1)
  in
  iter lst [] []

(**Recursively generates the environments (begin...end) statements*)
let calculate_environments lst =
  let rec extract_env acc lst =
    match lst with
    | [] -> (acc, [])
    | OneArgCmd (s, _, Line s1 :: _) :: q when String.equal s "begin" ->
        let env, l = extract_env [] q in
        let env = Env (s1, List.rev env) in
        extract_env (env :: acc) l
    | OneArgCmd (s, _, _) :: q when String.equal s "end" -> (acc, q)
    | Chapter (s, l) :: q ->
        let l2, q2 = extract_env [] l in
        let l2 = List.rev l2 in
        extract_env (Chapter (s, l2 @ q2) :: acc) q
    | Section (s, l) :: q ->
        let l2, q2 = extract_env [] l in
        let l2 = List.rev l2 in
        extract_env (Section (s, l2 @ q2) :: acc) q
    | Subsection (s, l) :: q ->
        let l2, q2 = extract_env [] l in
        let l2 = List.rev l2 in
        extract_env (Section (s, l2 @ q2) :: acc) q
    | Subsubsection (s, l) :: q ->
        let l2, q2 = extract_env [] l in
        let l2 = List.rev l2 in
        extract_env (Section (s, l2 @ q2) :: acc) q
    | e :: q -> extract_env (e :: acc) q
  in
  let a, _ = extract_env [] lst in
  a

(**Reads the preamble for type,title,author and eventual glossary input*)
let rec read_preamble ast =
  match ast with
  | [] -> ()
  | OneArgCmd (s, _, Line s1 :: _) :: q when s = "documentclass" ->
      Hashtbl.add preamble "type" s1;
      read_preamble q
  | OneArgCmd (s, _, Line s1 :: _) :: q when s = "input" ->
      Hashtbl.add preamble "glossary" s1;
      read_preamble q
  | OneArgCmd (s, _, Line s1 :: _) :: q when s = "title" ->
      Hashtbl.add preamble "title" s1;
      read_preamble q
  | OneArgCmd (s, _, Line s1 :: _) :: q when s = "author" ->
      Hashtbl.add preamble "author" s1;
      read_preamble q
  | MultipleArgCmd (s, _, l) :: q when s = "newcommand" -> (
      match l with
      | [ AtomicCmd (s, _) ] :: n :: _ when not (s = "sep") ->
          let env = calculate_environments n in
          Hashtbl.add commands s env;
          read_preamble q
      | _ -> read_preamble q)
  | _ :: q -> read_preamble q

(**Take the structure list and order of the Chapters,section etc.*)
let separate_sections lst =
  let tab = [| false; false; false; false |] in
  let rec extract_section acc lst =
    match lst with
    | [] -> (acc, [])
    | OneArgCmd (s, e, Line s1 :: _) :: q when s = "chapter" || s = "chapter*"
      ->
        if tab.(0) = true then (
          tab.(0) <- false;
          tab.(1) <- false;
          tab.(2) <- false;
          tab.(3) <- false;
          (acc, OneArgCmd (s, e, [ Line s1 ]) :: q))
        else (
          tab.(0) <- true;
          let a, l = extract_section [] q in
          let chap = Chapter (s1, List.rev a) in
          extract_section (chap :: acc) l)
    | OneArgCmd (s, e, Line s1 :: _) :: q when s = "section" || s = "section*"
      ->
        if tab.(1) = true then (
          tab.(1) <- false;
          tab.(2) <- false;
          tab.(3) <- false;
          (acc, OneArgCmd (s, e, [ Line s1 ]) :: q))
        else (
          tab.(1) <- true;
          let a, l = extract_section [] q in
          let chap = Section (s1, List.rev a) in
          extract_section (chap :: acc) l)
    | OneArgCmd (s, e, Line s1 :: _) :: q
      when s = "subsection" || s = "subsection*" ->
        if tab.(2) = true then (
          tab.(2) <- false;
          tab.(3) <- false;
          (acc, OneArgCmd (s, e, [ Line s1 ]) :: q))
        else (
          tab.(2) <- true;
          let a, l = extract_section [] q in
          let chap = Subsection (s1, List.rev a) in
          extract_section (chap :: acc) l)
    | OneArgCmd (s, e, Line s1 :: _) :: q
      when s = "subsubsection" || s = "subsubsection*" ->
        if tab.(3) = true then (
          tab.(3) <- false;
          (acc, OneArgCmd (s, e, [ Line s1 ]) :: q))
        else (
          tab.(3) <- true;
          let a, l = extract_section [] q in
          let chap = Subsubsection (s1, List.rev a) in
          extract_section (chap :: acc) l)
    | e :: q -> extract_section (e :: acc) q
  in
  let a, _ = extract_section [] lst in
  List.rev a
