let rec cmb xs n =
  if n = 0 then
    [[]]
  else
    match xs with
    | [] -> []
    | x::xs ->
       [
         List.map (fun l -> x::l) (cmb xs (n - 1));
         cmb xs n;
       ]
       |> List.concat
;;

let chooseBestSum(t: int) (k: int) (ls: int list): int =
  cmb ls k
  |> List.map (fun l -> List.fold_left (+) 0 l)
  |> List.filter (fun i -> i <= t)
  |> List.fold_left (fun i a -> if i > a then i else a) ~-1
;;

module Tests = struct
  (* let () = chooseBestSum 230 3 [91; 74; 73; 85; 73; 81; 87] |> print_int;; *)
  let%test _ = chooseBestSum 230 3 [91; 74; 73; 85; 73; 81; 87] = 228;;
  let%test _ = chooseBestSum 331 5 [91; 74; 73; 85; 73; 81; 87] = ~-1;;
end
