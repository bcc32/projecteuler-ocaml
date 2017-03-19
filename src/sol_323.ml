open Core

module M = struct
  let problem_number = 323

  let simulate_one bits =
    let end_ = Int.shift_left 1 bits in
    let mask = end_ - 1 in
    let x     = ref 0 in
    let count = ref 0 in
    while !x <> mask do
      incr count;
      let y = Random.int end_ in
      x := Int.bit_or !x y
    done;
    !count

  let simulate bits times =
    let sample_counts = Array.create 0 ~len:40 in
    for _ = 1 to times do
      let sample = simulate_one bits in
      Array.unsafe_set sample_counts sample (Array.unsafe_get sample_counts sample + 1)
    done;
    sample_counts

  let main () =
    Random.self_init ();
    let times = 100_000_000 in
    let bits = 2 in
    simulate bits times
    |> printf !"%{sexp: int array}\n"
end

include Solution.Make(M)
