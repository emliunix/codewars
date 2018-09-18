(* https://www.codewars.com/kata/longest-common-subsequence/train/ocaml *)
let lcs (s1: 'a list) (s2: 'a list) =
  let rec _rec s1 s2 =
    match (s1, s2) with
    | ([], _) -> (0, [])
    | (_, []) -> (0, [])
    | (x1::s1', x2::s2') when x1 = x2 ->
       let (l, c) = _rec s1' s2' in
       (l + 1, x1 :: c)
    | (_::s1', _::s2') ->
       let (l1, c1) = _rec s1 s2' in
       let (l2, c2) = _rec s1' s2 in
       if l1 > l2 then (l1, c1) else (l2, c2)
  in
  _rec s1 s2 |> snd
;;

module Tests = struct
  let%test _ = lcs ['a'] ['b'] = [];;
  let%test _ = lcs ['a';'b';'c';'d';'e';'f'] ['a';'b';'c'] = ['a';'b';'c'];;
  let%test _ = lcs ['a';'b';'c';'d';'e';'f'] ['a';'b';'c';'g'] = ['a';'b';'c'];;
end
