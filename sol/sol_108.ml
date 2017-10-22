open! Core
open! Import

module M = struct
  let problem = `Number 108

  let num_solutions n =
    if n % 1000 = 0 then Debug.eprintf "%d" n;
    let count = ref 2 in
    for i = n + 2 to 2 * n - 1 do
      if n * i mod (i - n) = 0
      then (incr count)
    done;
    !count
  ;;

  let main () =
    let rec loop n =
      if num_solutions n > 1000
      then n
      else loop (n + 1)
    in
    loop 1
    |> printf "%d\n"
  ;;
  (* 180180
     3.5m *)
end

include Solution.Make(M)
