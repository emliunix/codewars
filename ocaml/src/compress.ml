(* https://www.codewars.com/kata/99-problems-number-8-eliminate-consecutive-duplicates-in-a-list/train/ocaml *)
let compress l : int list =
  match l with
  | [] -> []
  | [a] -> [a]
  | a::xs ->
     match List.fold_left (fun (a, l) i ->
               if a = i then
                 (a, l)
               else
                 (i, i :: l)
             ) (a, [a]) xs with
     | (_, b) -> List.rev b;;


module Tests = struct
  open Printf
  let print_int_list l = List.iter (printf "%d ") l
  let%test _ = compress [] = [];;
  (* let () = print_int_list @@ compress [1;1;2;3];; *)
  let%test _ = compress [1;1;1;2;3] = [1;2;3];;
end
