open! Core
open! Import

module M = struct
  let problem = Number 407
  let limit = 10_000_000

  let main () =
    let ms = Option_array.create ~len:(limit + 1) in
    let somes = ref 0 in
    let all = ref 0 in
    for m = limit downto 2 do
      if debug && m mod 1000 = 0
      then
        Debug.eprintf !"%d %{Percent}" m (Percent.of_mult (float !somes /. float !all));
      Number_theory.Int.divisors ((m * m) - m)
      |> List.iter ~f:(fun n ->
        if m < n && n <= limit && not (Option_array.unsafe_is_some ms n)
        then (
          Option_array.unsafe_set_some ms n m;
          incr somes));
      incr all
    done;
    (* trivial case, 1^2 = 1 (mod n) *)
    for i = 2 to limit do
      if not (Option_array.unsafe_is_some ms i) then Option_array.unsafe_set_some ms i 1
    done;
    Option_array.unsafe_set_some ms 1 0;
    (* just in case *)
    if debug then Sexp.save "407.sexp" [%sexp (ms : int Option_array.t)];
    let sum = ref 0 in
    for i = 1 to limit do
      sum := !sum + Option_array.unsafe_get_some_exn ms i
    done;
    printf "%d\n" !sum
  ;;

  (* 39782849136421
     24.1107m *)
end

include Solution.Make (M)
