open! Core

module M = struct
  let problem_number = 315

  let digit_segments =
    (* segments are numbered:
       top, top left, top right, middle, bottom left, bottom right, bottom *)
    [| [ 0; 1; 2; 4; 5; 6 ]
     ; [ 2; 5 ]
     ; [ 0; 2; 3; 4; 6 ]
     ; [ 0; 2; 3; 5; 6 ]
     ; [ 1; 2; 3; 5 ]
     ; [ 0; 1; 3; 5; 6 ]
     ; [ 0; 1; 3; 4; 5; 6 ]
     ; [ 0; 1; 2; 5 ]
     ; [ 0; 1; 2; 3; 4; 5; 6 ]
     ; [ 0; 1; 2; 3; 5; 6 ] |]
  ;;

  let digit_segments =
    Array.map digit_segments ~f:(fun segments ->
      List.fold segments ~init:0 ~f:(fun ac x ->
        ac lor (1 lsl x)))
  ;;

  let distance s1 s2 = Int.popcount (s1 lxor s2)

  let transition_cost n1 n2 =
    let rec loop n1 n2 ac =
      match n1, n2 with
      | 0, 0 -> ac
      | n, 0 -> loop (n1 / 10) n2 (ac + distance 0 digit_segments.(n mod 10))
      | 0, n -> loop n1 (n2 / 10) (ac + distance 0 digit_segments.(n mod 10))
      | n1, n2 -> loop (n1 / 10) (n2 / 10)
                    (ac + distance
                            digit_segments.(n1 mod 10)
                            digit_segments.(n2 mod 10))
    in
    loop n1 n2 0
  ;;

  let digital_root n =
    let rec loop n ac =
      match n with
      | 0 -> ac
      | n -> loop (n / 10) (ac + n mod 10)
    in
    loop n 0
  ;;

  let digital_root_sequence n =
    Sequence.unfold ~init:(Some n) ~f:(function
      | None -> None
      | Some s ->
        let d = digital_root s in
        if d = s
        then Some (s, None)
        else Some (s, Some d))
  ;;

  let sam_cost sequence =
    sequence
    |> Sequence.map ~f:(transition_cost 0)
    |> Sequence.sum (module Int) ~f:(fun x -> 2 * x)
  ;;

  let max_cost sequence =
    let left  = Sequence.append (Sequence.singleton 0) sequence in
    let right = Sequence.append sequence (Sequence.singleton 0) in
    Sequence.zip left right
    |> Sequence.sum (module Int) ~f:(fun (x, y) -> transition_cost x y)
  ;;

  let main () =
    let diff = ref 0 in
    let primes = Euler.prime_sieve 20_000_000 in
    for i = 10_000_000 to 20_000_000 do
      if primes.(i)
      then (
        let seq = digital_root_sequence i in
        diff := !diff + sam_cost seq - max_cost seq)
    done;
    printf "%d\n" !diff
  ;;
  (* 13625242 1s *)
end

include Euler.Solution.Make(M)
