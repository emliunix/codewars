(* https://www.codewars.com/trainer/ocaml *)

(* let print_points vs =
 *   vs |> List.iter (function (a, b) -> Printf.printf "(%f, %f)\n" a b);; *)

let v_plus : float * float -> float * float -> float * float = fun va vb ->
  match (va, vb) with
  | ((a, b), (c, d)) -> (a +. c, b +. d);;

let solve(a: int) (b: int) (c: int) (alpha: int) (beta: int) (gamma: int): int list =
  let pi = 4.0 *. atan 1.0 in
  let build_vector len angle =
    let ang_in_r = (pi *. angle /. 180.0) in
    let x = len *. (cos ang_in_r) in
    let y = len *. (sin ang_in_r) in
    (x, y)
  in
  let va = build_vector (float_of_int a) (float_of_int alpha) in
  let vb = build_vector (float_of_int b) (float_of_int (beta + 90)) in
  let vc = build_vector (float_of_int c) (float_of_int (gamma + 180)) in
  let v = v_plus va vb |> v_plus vc in
  match v with
  | (a, b) ->
     let len = a *. a +. b *. b |> sqrt in
     let angle_cw =
       mod_float
         (atan (b /. a) *. 180.0 /. pi
          +. (if a < 0.0 then 180.0 else 0.0)
          +. 360.0)
          360.0
     in
     let angle = if angle_cw > 180.0 then 360.0 -. angle_cw else angle_cw in
     let deg = int_of_float angle in
     let minu = (angle -. float_of_int deg) *. 60.0 in
     let minu_int = int_of_float minu in
     let sec = (minu -. float_of_int minu_int) *. 60.0 in
     let sec_int = int_of_float sec in
     (* print_points [va; v_plus va vb; vb; v; vc]; *)
     [int_of_float (len +. 0.5); deg; minu_int; sec_int];;

module Tests = struct
  (* let%test _ =
   *   let it = solve 5 15 20 41 25 65 in
   *   it |> List.iter (Printf.printf "%d ");
   *   it = [11; 173; 31; 15];; *)
  let%test _ = solve 12 20 18 45 30 60 = [15; 135; 49; 18];;
  let%test _ = solve 5 15 20 41 25 65 = [11; 173; 31; 15];;
end
