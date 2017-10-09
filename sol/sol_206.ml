open! Core
open! Import
open Re2.Std

module M = struct
  let problem = `Number 206

  let main () =
    let pattern    = "1.2.3.4.5.6.7.8.9.0"  in
    let pattern_re = Re2.create_exn pattern in
    let sqrt_replace digit ~dir =
      String.tr pattern ~target:'.' ~replacement:digit
      |> Int.of_string
      |> Float.of_int
      |> sqrt
      |> Float.iround_exn ~dir
    in
    let lb = sqrt_replace '0' ~dir:`Down |> Int.round_down ~to_multiple_of:10 in
    let ub = sqrt_replace '9' ~dir:`Up   |> Int.round_up   ~to_multiple_of:10 in
    Sequence.range lb ub ~stop:`inclusive ~stride:10
    |> Sequence.find ~f:(fun n ->
      n * n |> Int.to_string |> Re2.matches pattern_re)
    |> Option.value_exn
    |> printf "%d\n"
  ;;
  (* 1389019170
     16s *)
end

include Solution.Make(M)
