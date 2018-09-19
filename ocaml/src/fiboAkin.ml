(* https://www.codewars.com/kata/fibo-akin *)
let u n =
  let arr = Array.make (n + 1) 1 in
  [0;1;1] |> List.iteri (fun i e -> Array.set arr i e);
  let rec _rec i =
    if i = n + 1 then ()
    else
      let n1 = Array.get arr (i - 2) in
      let n2 = Array.get arr (i - 1) in
      let v1 = Array.get arr (i - n1) in
      let v2 = Array.get arr (i - n2) in
      let v = v1 + v2 in
      (* Printf.printf "%d " v; *)
      Array.set arr i v;
      _rec (i + 1);
  in
  _rec 3;
  arr
;;

let lengthSupUK n k =
  u n
  |> Array.fold_left (
         fun a i ->
         a + if i >= k then 1 else 0
       ) 0
;;

let comp n =
  u n
  |> Array.fold_left (
         fun a i ->
         match a with
         | (last, count) ->
            (i, count + if i < last then 1 else 0)
       ) (0, 0)
  |> snd
;;

module Tests = struct
  let%test _ = lengthSupUK 50 25 = 2;;
  let%test _ = lengthSupUK 1745 645 = 474;;
  let%test _ = comp 11567 = 5683;;
  let%test _ = comp 23933 = 11856;;
end
