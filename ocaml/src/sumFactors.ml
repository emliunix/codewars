(* https://www.codewars.com/kata/sum-by-factors *)

let isPrime i =
  let bound = float_of_int i |> sqrt |> int_of_float in
  let rec _rec j =
    if j > bound then true else
      if i mod j = 0 then false else _rec (j + 1)
  in
  _rec 2
;;

let nextPrime n =
  let rec _rec i =
    if isPrime i then i else _rec (i + 1)
  in
  _rec (n + 1)
;;

let primeFactors n =
  let n = abs n in
  let rec _rec p =
    if p > n then
      []
    else
      if n mod p = 0 then
        p :: _rec (nextPrime p)
      else
        _rec (nextPrime p)
  in
  _rec 2
;;

let sum_of_divided (xs: int list): string =
  let xxs = List.map (fun x -> (x, primeFactors x)) xs in
  let rec _rec xxs =
    let xxs = xxs |> List.filter (function | (_, []) -> false
                                           | _ -> true)
    in
    match xxs with
    | [] -> []
    | _ ->
       let least =
         let firsts =
           xxs
           |> List.map (function (_, xs) -> List.hd xs)
         in
         match firsts with
         | [] -> raise @@ Failure "empty firsts"
         | x::[] -> x
         | x::xs -> List.fold_left min x xs
       in
       let (sum, xxs') =
         List.fold_left (fun a i (* : int * (int * int list) list *)->
             match a with
             | (sum, xxs') ->
                match i with
                | (_, []) -> (sum, xxs') (* not possible *)
                | (x, y::ys) when y = least ->
                   (sum + x, if ys = [] then xxs' else (x, ys)::xxs')
                | (x, ys) -> (sum, (x, ys)::xxs')
           ) (0, []) xxs
       in
       (least, sum) :: (_rec xxs')
  in
  _rec xxs
  |> List.map (function (a, sum) -> Printf.sprintf "(%d %d)" a sum)
  |> String.concat ""
;;

module Tests = struct
  let%test _ = sum_of_divided [12; 15] = "(2 12)(3 27)(5 15)";;
  let%test _ = sum_of_divided [15;21;24;30;45] = "(2 54)(3 135)(5 90)(7 21)";;
  let%test _ = sum_of_divided [107; 158; 204; 100; 118; 123; 126; 110; 116; 100] =
                 "(2 1032)(3 453)(5 310)(7 126)(11 110)(17 204)(29 116)(41 123)(59 118)(79 158)(107 107)";;
  let%test _ = sum_of_divided [-29804; -4209; -28265; -72769; -31744] =
                 "(2 -61548)(3 -4209)(5 -28265)(23 -4209)(31 -31744)(53 -72769)(61 -4209)(1373 -72769)(5653 -28265)(7451 -29804)";;
end
