(* https://www.codewars.com/kata/559b8e46fa060b2c6a0000bf/ *)
(* http://mathworld.wolfram.com/BinomialCoefficient.html *)
open Big_int

let diag (a:Big_int.big_int) n =
  let rec _r i =
    if i == 0 then [] else a :: (_r (i - 1))
  in
  _r n;;

let trans xs =
  match xs with
  | [] -> []
  | a :: xs ->
     let rec _tx p rs =
       match rs with
       | [] -> []
       | i :: _rs ->
          let n = (Big_int.add_big_int i p) in
          n :: (_tx n _rs)
     in
     a :: (_tx a xs);;

let rec n_trans n xs =
  if n == 0 then xs else n_trans (n - 1) (trans xs);;

let diagonal (n:int) (p:int) : string =
  let ls = (n_trans p (diag Big_int.unit_big_int (n - p + 1))) in
  let sum = List.fold_left (fun a b -> Big_int.add_big_int a b) Big_int.zero_big_int ls in
  Big_int.string_of_big_int sum;;
