(* https://www.codewars.com/kata/tank-truck/train/ocaml *)
let tankVol (h: int) (d: int) (vt: int) : int =
  let pi = 4.0 *. atan 1.0 in
  let h = float_of_int h in
  let d = float_of_int d in
  let r = d /. 2.0 in
  let vt = float_of_int vt in
  let theta = (r -. h) /. r |> acos in
  let s_tri = r *. r *. sin (2.0 *. theta) /. 2.0 in
  let s_circ = r *. r *. pi in
  let s_arc = s_circ *. theta /. pi in
  vt *. (s_arc -. s_tri) /. s_circ |> int_of_float;;

module Tests = struct
  let%test _ = tankVol 5 7 3848 = 2940;;
  let%test _ = tankVol 2 7 3848 = 907;;
  let%test _ = tankVol 2 8 5026 = 982;;
  let%test _ = tankVol 4 9 6361 = 2731;;
  let%test _ = tankVol 2 8 5026 = 982;;
end
