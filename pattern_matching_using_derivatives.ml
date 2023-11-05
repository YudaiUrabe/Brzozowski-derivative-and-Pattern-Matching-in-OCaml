(* Generating a regular expression randomly based on the formal definition of regular expression
  & Definition of alphabet:[a-z]*)

(* Definition of the type of regex:
r::=∅∣ε∣a∣r1r2∣r1+r2∣r*, a∈Σ    *)
type regex =
  | EmptySet
  | EmptyStr
  | Char of char
  | Concat of regex * regex
  | Union of regex * regex
  | Star of regex

let max_length = 5 (* Limiting the recursion depth to prevent the obtained regular expression from becoming infinitely long. *)

let rec generate_random_regex depth =
  if depth >= max_length then
    EmptyStr (*Recursion will finish here*)
  else
    let random_char () = Char (Char.chr (Random.int 26 + int_of_char 'a')) in
    match Random.int 10 with (* Define "random_char" by utilizing ASCII Code. To avoid generating too many empty strings or empty sets, I have reduced the probability of selecting them. *)
    | 0 -> EmptySet
    | 1 -> EmptyStr
    | 2 | 3-> random_char ()
    | 4 | 5-> Concat (generate_random_regex (depth+1), generate_random_regex (depth+1))
    | 6 | 7 -> Union (generate_random_regex (depth+1), generate_random_regex (depth+1))
    | _ -> Star (generate_random_regex (depth+1)) (* "| 8 | 9" is also OK instead of "_" *)
(*  Ex. If "Randome.int 10" equals 4, "random_char" will be a concatenation of two regex, which whill also be recursively defined.*)




(* Definition of Brzozowski derivative from (Brzozowski, 1964) Section3:
 Da(∅)=∅
 Da(ϵ)=∅
 Da(b)={ϵ (a=b) | ∅ (others)
 Da(P+Q)=Da(P)+Da(Q)
 Da(P⋅Q)={Da(P)⋅S + Da(Q) (ϵ∈L(P)) | Da(P)⋅Q (others)
 Da(P∗)=Da(P)⋅P∗  
*)

let rec brzozowski_derivative regex c = 
  match regex with
  | EmptySet -> EmptySet
  | EmptyStr -> EmptySet
  | Char x -> if x = c then EmptyStr else EmptySet
  | Concat (r1, r2) -> 
      if nullable r1 then (* ε ∈ L(r1) *)
        Union (Concat (brzozowski_derivative r1 c, r2), brzozowski_derivative r2 c)
      else (* ε ∉ L(r1) *)
        Concat (brzozowski_derivative r1 c, r2)
  | Union (r1, r2) -> Union (brzozowski_derivative r1 c, brzozowski_derivative r2 c)
  | Star r -> Concat (brzozowski_derivative r c, Star r)

(* Check if the regex can generate empty string or not. The function "nullable" is mutually recursive with "brzozowski_derivative". *)
and nullable regex =  (* cf.(Brzozowski,1964),p.482,Def3.2, the δ-function*)
  match regex with
  | EmptySet -> false
  | EmptyStr -> true
  | Char _ -> false
  | Concat (r1, r2) -> nullable r1 && nullable r2
  | Union (r1, r2) -> nullable r1 || nullable r2
  | Star _ -> true (*Star represents "zero" or more iterations, so it can generate the empty string!*)

(*  Convert a regular expression to a string, creating strings like (a|b)*. *)
  let rec string_of_regex regex =
  match regex with
  | EmptySet -> "φ"
  | EmptyStr -> "ε"
  | Char c -> String.make 1 c (* If the regex is a character c, it returns a single-length string c *)
  | Concat (r1, r2) -> string_of_regex r1 ^ string_of_regex r2
  | Union (r1, r2) -> "(" ^ string_of_regex r1 ^ "|" ^ string_of_regex r2 ^ ")"
  | Star r -> "(" ^ string_of_regex r ^ ")*"


(* Calculate the Brzozowski derivative using rendomly generated regular expression "random_regex" and an input symbol "input_char",
 and then check if the "input_char" is contained in the language represented by "regex_str".*)
let () =
  Random.self_init ();
  let random_regex = generate_random_regex 0 in
  let regex_str = string_of_regex random_regex in(* Convert "random_regex", a regular expression, to a string *)
  print_endline ("Randomly Generated Regular Expression: " ^ regex_str);

  print_endline "Enter a single small character[a-z]:";
  let input_char = input_char stdin in(* I will modify here and related parts to handle a string *)
  (* print_endline ("You entered: " ^ (String.make 1 input_char));  *)

  let derivative = brzozowski_derivative random_regex input_char in
  let derivative_str = string_of_regex derivative in (*Convert "derivative", which is a regular expression, to a string*)
  print_endline ("the Brzozowski derivative of \""^regex_str^ "\" with respect to \"" ^(String.make 1 input_char)^ "\" is \n\"" ^ derivative_str^"\"");


      (* Replace EmptySet with Char c for pattern matching. a little redundant *)
  let rec brzozowski_derivative_without_EmptySet regex c = 
    match regex with
    | EmptySet -> Char c
    | EmptyStr -> EmptySet
    | Char x -> if x = c then EmptyStr else EmptySet
    | Concat (r1, r2) -> 
        if nullable r1 then (* ε ∈ L(r1) *)
          Union (Concat (brzozowski_derivative_without_EmptySet r1 c, r2), brzozowski_derivative_without_EmptySet r2 c)
        else (* ε ∉ L(r1) *)
          Concat (brzozowski_derivative_without_EmptySet r1 c, r2)
    | Union (r1, r2) -> Union (brzozowski_derivative_without_EmptySet r1 c, brzozowski_derivative_without_EmptySet r2 c)
    | Star r -> Concat (brzozowski_derivative_without_EmptySet r c, Star r)



      (* match derivative with  
  | EmptyStr -> print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" matches the regular expression \"" ^ regex_str ^ "\"")
  | _ -> print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" does \"not\" match the regular expression \"" ^ regex_str ^ "\"") *)

  let derivative_without_EmptySet = brzozowski_derivative_without_EmptySet random_regex input_char in

  if nullable derivative_without_EmptySet then  (* Pattern matching: Checking if the given regular expression accepts a character (or string), cf.(Brzozowski,1964),Thm4.2. *)
    print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" matches the regular expression \"" ^ regex_str ^ "\"")
  else
    print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" does \"not\" match the regular expression \"" ^ regex_str ^ "\"")

    (* Replace EmptySet with Char c for pattern matching. a little redundant *)
    let rec brzozowski_derivative_without_EmptySet regex c = 
      match regex with
      | EmptySet -> Char c
      | EmptyStr -> EmptySet
      | Char x -> if x = c then EmptyStr else EmptySet
      | Concat (r1, r2) -> 
          if nullable r1 then (* ε ∈ L(r1) *)
            Union (Concat (brzozowski_derivative_without_EmptySet r1 c, r2), brzozowski_derivative_without_EmptySet r2 c)
          else (* ε ∉ L(r1) *)
            Concat (brzozowski_derivative_without_EmptySet r1 c, r2)
      | Union (r1, r2) -> Union (brzozowski_derivative_without_EmptySet r1 c, brzozowski_derivative_without_EmptySet r2 c)
      | Star r -> Concat (brzozowski_derivative_without_EmptySet r c, Star r)
  
  
  
        (* match derivative with  
    | EmptyStr -> print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" matches the regular expression \"" ^ regex_str ^ "\"")
    | _ -> print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" does \"not\" match the regular expression \"" ^ regex_str ^ "\"") *)
  
    let derivative_without_EmptySet = brzozowski_derivative_without_EmptySet random_regex input_char in
  
    if nullable derivative_without_EmptySet then  (* Pattern matching: Checking if the given regular expression accepts a character (or string), cf.(Brzozowski,1964),Thm4.2. *)
      print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" matches the regular expression \"" ^ regex_str ^ "\"")
    else
      print_endline ("The symbol \"" ^ (String.make 1 input_char) ^ "\" does \"not\" match the regular expression \"" ^ regex_str ^ "\"")
  