let rec at k ls =
  match ls with
  | [] -> None
  | x::xs ->
     if k = 0 then
       Some x
     else
       at (k - 1) xs;;

module Tests = struct
  let%test _ = at 0 [1; 2; 3] = Some 1;;
  let%test _ = at 1 [] = None;;
end
