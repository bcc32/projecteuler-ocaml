open Core.Std

module M = struct
  let problem_number = 214

  let limit = 40_000_000

  let totient_chain_length =
    lazy (
      let table = Array.create None ~len:(limit + 1) in
      table.(1) <- Some 1;
      let rec totient_chain_length n =
        match Array.unsafe_get table n with
        | None ->
          let result = n |> Euler.Int.totient |> totient_chain_length |> succ in
          Array.unsafe_set table n (Some result);
          result
        | Some n -> n
      in
      totient_chain_length
    )

  let main () =
    let totient_chain_length = force totient_chain_length in
    Euler.prime_sieve limit
    |> Array.foldi ~init:0 ~f:(fun i acc is_prime ->
      if is_prime && totient_chain_length i = 25
      then acc + i
      else acc)
    |> printf "%d\n"
end

include Solution.Make(M)
