let get_int s =
  String.sub s 1 (String.length s - 1)
  |> int_of_string;;

let list_ends l =
  let first = List.hd l in
  let rec get_last = function
    | x::[] -> x
    | _::xs -> get_last xs
    | [] -> raise @@ Invalid_argument "last"
  in
  (first, get_last l);;

let list_assoc_opt a l =
  try Some (List.assoc a l)
  with Not_found -> None;;

let fill_towns (friends : string array) (towns : string array array) =
  let
    towns = Array.to_list towns
            |> List.map (fun t_pair ->
                   match Array.to_list t_pair with
                   | [a;b] -> (a, b)
                   | _ -> raise (Invalid_argument "towns not valid")
                 )
  in
  friends |> Array.to_list
  |> List.map (fun f -> (f, list_assoc_opt f towns))
  |> List.filter (function
         | (_, Some _) -> true
         | (_, None) -> false)
  |> List.map (function
         | (f, Some t) -> (f, t)
         | _ -> raise (Invalid_argument "should not happen")
       );;

let rec pair_towns towns =
  match towns with
  | [] -> []
  | _::[] -> []
  | t0::((t1::_) as _towns) ->
     (t0, t1) :: pair_towns _towns;;

let tour(arrFriends: string array) (ftwns: string array array) (h: (string * float) list): int =
  let friends_with_towns = fill_towns arrFriends ftwns in
  let towns = List.map (function (_, t) -> t) friends_with_towns in
  let towns_paired = pair_towns towns in
  let towns_sorted = List.sort (fun t1 t2 -> (get_int t1) - (get_int t2)) towns in
  let towns_sorted_paired = pair_towns towns_sorted in
  let dists =
    List.map (function (from, _to) ->
                let l0 = List.assoc from h in
                let l1 = List.assoc _to h in
                let l = sqrt @@ l1 *. l1 -. l0 *. l0 in
                ((from, _to), l)
      ) towns_sorted_paired
  in
  let dist_of_2_town (t1, t2) =
    let (t1, t2) = if get_int t1 > get_int t2 then (t2, t1) else (t1, t2) in
    let rec helper is_inside pairs acc =
      match pairs with
      | [] -> acc
      | ((from, _to) as p)::_pairs ->
         let _is_inside = is_inside || t1 = from in
         let _acc =
           if _is_inside then
              acc +. (List.assoc p dists)
            else
              acc
         in
         if _to = t2 then
           _acc
         else
           helper _is_inside _pairs _acc
    in
    helper false towns_sorted_paired 0.0
  in
  let dist_float = match towns with
    | [] -> 0.0
    | _ ->
       let (first_town, last_town) = list_ends towns in
       let other_dist =
         List.map dist_of_2_town towns_paired
         |> List.fold_left (+.) 0.0
       in
       List.assoc first_town h
       +. List.assoc last_town h
       +. other_dist
  in
  int_of_float dist_float;;

module Tests = struct
  let%test _ =
    let friends = [|"A1"; "A2"; "A3"; "A4"; "A5"|] in
    let ftowns = [| [|"A1"; "X1"|]; [|"A2"; "X2"|]; [|"A3"; "X3"|]; [|"A4"; "X4"|] |] in
    let dists = [("X1", 100.0); ("X2", 200.0); ("X3", 250.0); ("X4", 300.0)] in
    tour friends ftowns dists = 889;;
  let%test _ =
    let friends = [|"A1"; "A2"; "A3"; "A4"; "A5"|] in
    let ftowns = [| [|"A1"; "X1"|]; [|"A2"; "X2"|]; [|"A3"; "X3"|]; [|"A4"; "X4"|]; [|"A5"; "X5"|] |] in
    let dists = [("X1", 100.0); ("X2", 200.0); ("X3", 250.0); ("X4", 300.0); ("X5", 320.0)] in
    tour friends ftowns dists = 1020;;
end
