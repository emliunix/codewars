(* https://www.codewars.com/kata/56efab15740d301ab40002ee/train/ocaml *)

let oper fct arr init : int list =
  let rec helper a inarr res =
    match inarr with
    | [] -> res
    | x::xs -> let v = fct a x in
                helper v xs (v :: res)
  in
  List.rev @@ helper init arr []

let som (x: int) (y: int) = x + y;;

let gcdi u v =
  let rec helper u v =
    let m = u mod v in
    if m = 0 then v else helper v m
  in
  let au = (abs u) and
      av = (abs v) in
  if au > av then
    helper au av
  else
    helper av au;;

let lcmu m n =
  let am = abs m and
      an = abs n in
  am * an / (gcdi am an)
;;

let maxi x y = if x > y then x else y;;

let mini x y = if x > y then y else x;;

module Tests = struct
  (* "gcdi a = [ 18; 69; -90; -78; 65; 40 ]" *)
  let a = [ 18; 69; -90; -78; 65; 40 ];;
  let%test _ =
    oper gcdi a (List.hd a) = [ 18; 3; 3; 3; 1; 1 ];;
  let%test _ =
    oper lcmu a (List.hd a) = [ 18; 414; 2070; 26910; 26910; 107640 ];;
  let%test _ =
    oper som a 0 = [ 18; 87; -3; -81; -16; 24 ];;
  let%test _ =
    oper mini a (List.hd a) = [ 18; 18; -90; -90; -90; -90 ];;
  let%test _ =
    oper maxi a (List.hd a) = [ 18; 69; 69; 69; 69; 69 ];;
end
