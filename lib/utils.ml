module Utils = struct
let string_starts_with prefix str =
  if String.length prefix > String.length str then false else 
    let a = String.sub str 0 (String.length prefix) in String.equal a prefix;;
let string_to_list = fun c -> List.of_seq (String.to_seq c);;
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      let a = input_line chan in lines := a :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
  end;;