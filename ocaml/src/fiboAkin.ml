(* https://www.codewars.com/kata/fibo-akin *)
let u n =
  let arr = DynArray.make 3 in
  [0;1;1] |> List.iter (fun i -> DynArray.add arr i);
  let rec _rec i =
    if i = n then ()
    else
      let n1 = DynArray.get arr (i - 2) in
      let n2 = DynArray.get arr (i - 1) in
      let v1 = DynArray.get arr (i - n1) in
      let v2 = DynArray.get arr (i - n2) in
      let v = v1 + v2 in
      Printf.printf "%d " v;
      DynArray.add arr v;
      _rec (i + 1);
  in
  _rec 3
;;


let () = u 1000;;
