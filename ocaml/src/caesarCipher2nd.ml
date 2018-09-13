(* http://www.codewars.com/kata/55084d3898b323f0aa000546/train/ocaml *)

let ca = Char.code 'a';;
let cA = Char.code 'A';;

let test_char = function
  | c when c >= 'a' && c <= 'z' -> `LowerCase
  | c when c >= 'A' && c <= 'Z' -> `UpperCase
  | _ -> `Other;;

let rec split_parts s part_len =
  match s with
  | "" -> []
  | _ ->
     let cut_len = min part_len (String.length s) in
     let remain_len = String.length s - cut_len in
     (String.sub s 0 cut_len) :: split_parts (String.sub s cut_len remain_len) part_len;;

let shift_char c shift =
  let _shift base = (Char.code c - base + shift) mod 26 + base |> Char.chr in
  match test_char c with
  | `LowerCase -> _shift ca
  | `UpperCase -> _shift cA
  | `Other -> c
;;

let encode (s: string) (shift: int): string list =
  let len_s = String.length s in
  let buffer = Buffer.create @@ len_s + 2 in
  let first_char = Char.lowercase s.[0] in
  let two_letter_key = Printf.sprintf "%c%c" first_char (shift_char first_char shift)
  let part_len = (float_of_int len_s) /. 5.0 |> ceil |> int_of_float in
  [""];;

let decode (a: string list): string = "";;
