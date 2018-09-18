(* let rec cmb = function
 *   | [] -> [[]]
 *   | x::xs ->
 *      cmb xs
 *      |> List.map (fun l -> [x::l; l])
 *      |> List.concat
 * ;; *)

let divisors i =
  let rec _rec j =
    if j > i then
      []
    else
      if i / j * j = i then
        j :: _rec (j + 1)
      else
        _rec (j + 1)
  in
  _rec 1
;;

let list_squared (m: int) (n: int): string =
  let buf = Buffer.create 100 in
  let calc_i i =
    divisors i
    |> List.map (fun i -> i * i)
    |> List.fold_left (+) 0
    |> (fun i ->
      let i' = sqrt (float_of_int i) |> int_of_float in
      if i' * i' = i then
        Some i
      else
        None
    )
  in
  let rec _rec i first_item =
    if i = n then
      ()
    else
      match calc_i i with
      | Some i2 ->
         (
           if not first_item then
             Buffer.add_string buf "; "
           else
             ()
         );
         Printf.sprintf "(%d, %d)" i i2
         |> Buffer.add_string buf;
         _rec (i + 1) false
      | None -> _rec (i + 1) first_item
  in
  _rec m true;
  Buffer.contents buf
;;


module Tests = struct
  let%test _ = list_squared 1 250 = "(1, 1); (42, 2500); (246, 84100)";;
  let%test _ = list_squared 42 250 = "(42, 2500); (246, 84100)";;
end
