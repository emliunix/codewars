(* Consecutive strings *)
(* https://www.codewars.com/kata/56a5d994ac971f1ac500003e/train/ocaml *)

let listTake n list =
  let rec _rec i xs =
    if i < n then
      match xs with
      | x::xs -> x :: _rec (i + 1) xs
      | [] -> raise @@ Invalid_argument "list"
    else
      []
  in
  _rec 0 list;;

let listDrop n list =
  let rec _rec i xs =
    if i < n then
      match xs with
      | _::xs -> _rec (i + 1) xs
      | [] -> raise @@ Invalid_argument "list"
    else
      xs
  in
  _rec 0 list;;

let longestConsec (xs: string list) (k: int): string =
  let maxI = List.length xs - k in
  let rec _rec i =
    let rs = listDrop i xs in
    let ls = listTake k rs in
    let str = String.concat "" ls in
    if i < maxI then
      str :: _rec (i + 1)
    else
      [str]
  in
  let str_ls = _rec 0 in
  let str_len_ls = str_ls |> List.map String.length in
  let maxi xs =
    let rec _rec i xs maxI maxVal =
      match xs with
      | [] -> maxI
      | x::xs ->
         if x > maxVal then
           _rec (i + 1) xs i x
         else
           _rec (i + 1) xs maxI maxVal
    in
    match xs with
    | [] -> None
    | x::xs -> Some (_rec 1 xs 0 x)
  in
  let max_len_i = maxi str_len_ls in
  match max_len_i with
  | None -> ""
  | Some i -> List.nth str_ls i
;;

module Tests = struct
  let%test _ = longestConsec ["zone";"abigail";"theta";"form";"libe";"zas";"theta";"abigail"] 2 = "abigailtheta";;
  let%test _ = longestConsec ["ejjjjmmtthh";"zxxuueeg";"aanlljrrrxx";"dqqqaaabbb";"oocccffuucccjjjkkkjyyyeehh"] 1 = "oocccffuucccjjjkkkjyyyeehh";;
  let%test _ = longestConsec ["itvayloxrp";"wkppqsztdkmvcuwvereiupccauycnjutlv";"vweqilsfytihvrzlaodfixoyxvyuyvgpck"] 2 = "wkppqsztdkmvcuwvereiupccauycnjutlvvweqilsfytihvrzlaodfixoyxvyuyvgpck";;
  let%test _ = longestConsec [] 3 = "";;
  let%test _ = longestConsec ["it";"wkppv";"ixoyx";"3452";"zzzzzzzzzzzz"] 15 = "";;
  let%test _ = longestConsec ["it";"wkppv";"ixoyx";"3452";"zzzzzzzzzzzz"] 0 = "";;
end
