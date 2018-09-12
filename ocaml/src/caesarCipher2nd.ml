(* http://www.codewars.com/kata/55084d3898b323f0aa000546/train/ocaml *)

let ca = Char.code 'a';;
let cA = Char.code 'A';;

let test_char = function
  | c when c >= 'a' && c <= 'z' -> `LowerCase
  | c when c >= 'A' && c <= 'Z' -> `UpperCase
  | _ -> `Other;;

let rec split_parts part_len s =
  match s with
  | "" -> []
  | _ ->
     let cut_len = min part_len (String.length s) in
     let remain_len = String.length s - cut_len in
     (String.sub s 0 cut_len) :: split_parts part_len (String.sub s cut_len remain_len);;

let shift_char shift c =
  let _shift base = (Char.code c - base + shift) mod 26 + base |> Char.chr in
  match test_char c with
  | `LowerCase -> _shift ca
  | `UpperCase -> _shift cA
  | `Other -> c
;;

let encode (s: string) (shift: int): string list =
  let len_s = String.length s in
  let part_len = (float_of_int (len_s + 2)) /. 5.0 |> ceil |> int_of_float in
  let buffer = Buffer.create @@ len_s + 2 in
  let first_char = Char.lowercase_ascii s.[0] in
  Buffer.add_char buffer first_char;
  Buffer.add_char buffer (shift_char shift first_char);
  String.map (shift_char shift) s |> Buffer.add_string buffer;
  Buffer.contents buffer
  |> split_parts part_len
;;

let decode (a: string list): string =
  let full_msg = String.concat "" a in
  let header = String.sub full_msg 0 2 in
  let body = String.sub full_msg 2 (String.length full_msg - 2) in
  let shift = 26 - (Char.code header.[1] + 26 - Char.code header.[0]) mod 26 in
  let msg = String.map (shift_char shift) body in
  msg
;;

module Tests = struct
  let%test _ =
      let text = "I should have known that you would have a perfect answer for me!!!" in
      let shift = 1 in
      let encoded = [
          "ijJ tipvme ibw";
          "f lopxo uibu z";
          "pv xpvme ibwf ";
          "b qfsgfdu botx";
          "fs gps nf!!!"
        ]
      in
         encode text shift = encoded
      && decode encoded = text;;
end
