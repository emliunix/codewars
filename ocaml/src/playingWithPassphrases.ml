(* https://www.codewars.com/kata/playing-with-passphrases *)
let c_0 = Char.code '0';;
let c_9 = Char.code '9';;
let c_a = Char.code 'a';;
let c_z = Char.code 'z';;
let c_A = Char.code 'A';;
let c_Z = Char.code 'Z';;

let str_to_list s =
  let len = String.length s in
  let rec helper idx s =
    if idx = len then []
    else s.[idx] :: helper (idx + 1) s
  in
  helper 0 s;;

type testCase =
  | AlphaUpper
  | AlphaLower
  | Digit
  | Other;;

let test_char c =
  match Char.code c with
  | n when n >= c_0 && n <= c_9 -> Digit
  | n when n >= c_a && n <= c_z -> AlphaLower
  | n when n >= c_A && n <= c_Z -> AlphaUpper
  | _ -> Other;;

let complement_9 c =
  (9 - (Char.code c - c_0)) + c_0
  |> Char.chr;;

let shift_char c shift a =
  a + (Char.code c - a + shift) mod 26
  |> Char.chr;;

let play_pass s shift =
  let buf = Buffer.create (String.length s) in
  s
  |> str_to_list
  |> List.mapi (fun idx c ->
         let case_change =
           if idx mod 2 = 0
           then Char.uppercase_ascii
           else Char.lowercase_ascii
         in
         match test_char c with
         | AlphaUpper -> shift_char c shift c_A |> case_change
         | AlphaLower -> shift_char c shift c_a |> case_change
         | Digit -> complement_9 c
         | Other -> c)
  |> List.rev
  |> List.iter (fun c ->
         Buffer.add_char buf c
       );
  Buffer.contents buf;;

module Tests = struct
  (* open Printf *)
  (* let () = printf "%s" (play_pass "I LOVE YOU!!!" 1);; *)
  let%test _ = play_pass "I LOVE YOU!!!" 1 = "!!!vPz fWpM J";;
  let%test _ = play_pass "I LOVE YOU!!!" 0 = "!!!uOy eVoL I";;
  let%test _ = play_pass "AAABBCCY" 1 = "zDdCcBbB";;
  let%test _ = play_pass "MY GRANMA CAME FROM NY ON THE 23RD OF APRIL 2015" 2
                 = "4897 NkTrC Hq fT67 GjV Pq aP OqTh gOcE CoPcTi aO";;
end
