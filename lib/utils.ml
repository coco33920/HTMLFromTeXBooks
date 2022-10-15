(**Various utilities module*)

(**Implementation of 4.13's OCaml String.starts_with command to be backward compatible*)
let string_starts_with prefix str =
  if String.length prefix > String.length str then false
  else
    let a = String.sub str 0 (String.length prefix) in
    String.equal a prefix

(**A simple function to transform a string into a list of its chars*)
let string_to_list c = List.of_seq (String.to_seq c)

(**Simple re-usable file-reading into a list of line function*)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      let a = input_line chan in
      lines := a :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(**A function to write in a output file*)
let write_to_file file str =
  let f = open_out file in
  output_string f str;
  flush f;
  close_out f
