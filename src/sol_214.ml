open Core.Std

module M = struct
  let problem_number = 214

  let limit = 40_000_000

  let rec totient_chain_length =
    let table = Array.create None ~len:(limit + 1) in
    table.(1) <- Some 1;
    fun n ->
      match Array.unsafe_get table n with
      | None ->
        let result = n |> Euler.Int.totient |> totient_chain_length |> succ in
        Array.unsafe_set table n (Some result);
        result
      | Some n -> n

  let main () =
    Euler.prime_sieve limit
    |> Array.foldi ~init:0 ~f:(fun i acc is_prime ->
      if is_prime && totient_chain_length i = 25
      then acc + i
      else acc)
    |> printf "%d\n"
end

include Solution.Make(M)
