
type regex =
  | EmptySet
  | EmptyStr
  | Char of char
  | Concat of regex * regex
  | Union of regex * regex
  | Star of regex

let max_length = 5

let rec generate_random_regex depth =
  if depth >= max_length then
    EmptyStr
  else
    let random_char () = Char (Char.chr (Random.int 26 + int_of_char 'a')) in
    match Random.int 10 with
    | 0 -> EmptySet
    | 1 -> EmptyStr
    | 2 | 3-> random_char ()
    | 4 | 5-> Concat (generate_random_regex (depth+1), generate_random_regex (depth+1))
    | 6 | 7 -> Union (generate_random_regex (depth+1), generate_random_regex (depth+1))
    | _ -> Star (generate_random_regex (depth+1))

let rec brzozowski_derivative regex c = 
  match regex with
  | EmptySet -> EmptySet
  | EmptyStr -> EmptySet
  | Char x -> if x = c then EmptyStr else EmptySet
  | Concat (r1, r2) -> 
      if nullable r1 then
        Union (Concat (brzozowski_derivative r1 c, r2), brzozowski_derivative r2 c)
      else
        Concat (brzozowski_derivative r1 c, r2)
  | Union (r1, r2) -> Union (brzozowski_derivative r1 c, brzozowski_derivative r2 c)
  | Star r -> Concat (brzozowski_derivative r c, Star r)


and nullable regex =
  match regex with
  | EmptySet -> false
  | EmptyStr -> true
  | Char _ -> false
  | Concat (r1, r2) -> nullable r1 && nullable r2
  | Union (r1, r2) -> nullable r1 || nullable r2
  | Star _ -> true

  let rec string_of_regex regex =
  match regex with
  | EmptySet -> "φ"
  | EmptyStr -> "ε"
  | Char c -> String.make 1 c
  | Concat (r1, r2) -> string_of_regex r1 ^ string_of_regex r2
  | Union (r1, r2) -> "(" ^ string_of_regex r1 ^ "|" ^ string_of_regex r2 ^ ")"
  | Star r -> "(" ^ string_of_regex r ^ ")*"

let () =
  Random.self_init ();
  let random_regex = generate_random_regex 0 in
  let regex_str = string_of_regex random_regex in
  print_endline ("Randomly Generated Regular Expression: " ^ regex_str);

  print_endline "Enter a single small character[a-z]:";
  let input_char = input_char stdin in

  let derivative = brzozowski_derivative random_regex input_char in
  let derivative_str = string_of_regex derivative in
  print_endline ("the Brzozowski derivative of \""^regex_str^ "\" with respect to \"" ^(String.make 1 input_char)^ "\" is \n\"" ^ derivative_str^"\"");


  if nullable derivative then
    print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" matches the regular expression \"" ^ regex_str ^ "\"")
  else
    print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" does \"not\" match the regular expression \"" ^ regex_str ^ "\"")

