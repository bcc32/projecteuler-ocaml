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
          Number_theory.Int.iter_digits (k * n) ~f:(fun d ->
            if d = 0 || digits_used.(d)
            then raise Exit
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

module M = struct
  let problem = Number 38
  let main () = pandigital_multiples () |> printf "%d\n"

  (* 1.815ms *)
  let%expect_test "answer" =
    main ();
    [%expect {| 932718654 |}]
  ;;
end

include Solution.Make (M)
