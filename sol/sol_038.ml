open! Core
open! Import

let reify k n =
  if debug then Debug.eprintf "k = %d, n = %d" k n;
  List.range 1 n
  |> List.map ~f:(fun n -> k * n |> Int.to_string)
  |> String.concat
  |> Int.of_string
;;

let pandigital_multiples () =
  let max = ref 0 in
  let digits_used = Array.create false ~len:10 in
  for k = 1 to 9999 do
    if debug then Debug.eprintf "k = %d" k;
    Array.fill digits_used false ~pos:0 ~len:10;
    let digits_filled = ref 0 in
    let rec loop n =
      if !digits_filled = 9
      then max := Int.max !max (reify k n)
      else (
        try
          Number_theory.Int.As_base10.iter (k * n) ~f:(fun d ->
            if d = 0 || digits_used.(d)
            then Exn.raise_without_backtrace Exit
            else (
              digits_used.(d) <- true;
              incr digits_filled));
          loop (n + 1)
        with
        | Exit -> ())
    in
    loop 1
  done;
  !max
;;

let main () = pandigital_multiples () |> printf "%d\n"

(* 1.815ms *)
let%expect_test "answer" =
  main ();
  [%expect {| 932718654 |}]
;;

include (val Solution.make ~problem:(Number 38) ~main)
