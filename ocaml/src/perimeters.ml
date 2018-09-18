(* uncomment if you need it: #load "nums.cma";; open Num;; 
   perimeter returns a big integer as a string*)

(* This program requires a lower version of OCaml
 * #load "nums.cma";;
 * open Num;;
 * 
 * let perimeter (n: int): string =
 *   Big_int.(
 *     let rec _rec n1 n2 i sum =
 *       if i = n + 1 then
 *         sum
 *       else
 *         let n3 = add_big_int n1 n2 in
 *         let newsum = add_big_int sum n3 in
 *         _rec n2 n3 (i + 1) newsum
 *     in
 *     _rec
 *       (big_int_of_int 1)
 *       (big_int_of_int 1)
 *       2
 *       (Big_int.big_int_of_int 2)
 *     |> (fun i -> Big_int.shift_left_big_int i 2)
 *     |> Big_int.string_of_big_int
 *   )
 * ;;
 * 
 * module Tests = struct
 *   let%test _ = perimeter 5 = "80";;
 *   let%test _ = perimeter 7 = "216";;
 *   let%test _ = perimeter 20 = "114624";;
 *   let%test _ = perimeter 30 = "14098308";;
 * end *)
