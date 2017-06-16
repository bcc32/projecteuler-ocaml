open Core

module M = struct
  let problem_number = 131

  (* We want to find primes [p] where there exists some positive integer, [n],
     such that [n^3 + n^2 p = m^3] for some integer [m]. The problem statement
     declares that such an [n] will be unique.

     We can factor the original expression as [n^2(n + p) = m^3].

     Based on prime factorization, either:

     (a) [n^2] is a cube, which implies [n] is a cube, and [n + p] must also be
     a cube.

     (b) [n^2] is not a cube, which means [n + p] must have the right prime
     factors to make [n^2(n + p)] a cube. This in turn means that [n + p] must
     have the same set of prime factors as [n^2] (equivalently [n]). Since [n +
     p > n], this cannot happen unless [n + p] is an integer multiple of [n],
     but this is impossible because [p] is prime and therefore not divisible by
     [n] ([n > 1] by case-work).

     So we can adopt the following strategy for finding the value of [n], if it
     exists, for a given [p]: iterate through perfect cube values of [n],
     starting at 1. For each [n], check whether [n + p] is a cube; if so, return
     [n]. Otherwise, continue until [n + p] is less than the next cube after
     [n]; since the gaps between cubes increases monotonically, the values of
     [n] we need to check are bounded quite reasonably. *)


  (* The maximum value of [p] we need to check is less than [1_000_000], so we
     only need to continue until the gaps between cubes becomes as large, i.e.,
     when the cube root is [600] or so. *)
  let cubes =
    Sequence.range 0 600 ~stop:`inclusive
    |> Sequence.map ~f:(fun x -> x * x * x)
    |> Sequence.to_array


  let find_n p =
    with_return (fun { return } ->
      for x = 0 to Array.length cubes - 1 do
        let i =
          Array.binary_search cubes
            ~pos:(x + 1)
            ~len:(Array.length cubes - (x + 1))
            ~compare:Int.compare
            `Last_less_than_or_equal_to
            (cubes.(x) + p)
        in
        match i with
        | None -> return None
        | Some i when cubes.(i) = cubes.(x) + p -> return (Some cubes.(i))
        | _ -> ()
      done;
      assert false)

  let main () =
    Euler.Int.primes
    |> Sequence.take_while ~f:(fun p -> p < 1_000_000)
    |> Sequence.count ~f:(fun p -> Option.is_some (find_n p))
    |> printf "%d\n"

  (* After reading a bit more on the ProjectEuler forum, I discovered that
     because [p] is a difference of cubes, that:

     [a^3 - b^3 = (a - b)(a^2 + ab + b^2)]

     implies that [p] must be the difference between two /consecutive/ cubes
     (i.e., [a - b = 1]). Therefore, we can simplify substantially. *)

  let _ = main

  let main () =
    let count = ref 0 in
    for i = 1 to 576 do
      let j = i + 1 in
      if Euler.Int.is_prime (j * j * j - i * i * i)
      then (incr count)
    done;
    printf "%d\n" !count

  (* This is about 4000x faster than the previous solution. *)
end
include Solution.Make(M)
