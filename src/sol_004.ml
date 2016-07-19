open Core.Std

module M = struct
  let problem_number = 4

  let main () =
    let ans = ref 0 in
    for i = 100 to 999 do
      for j = 100 to 999 do
        let digits = i * j |> Euler.digits_of_int in
        if Euler.is_palindrome ~equal:Int.equal digits && i * j > !ans
        then ans := i * j
      done
    done;
    printf "%d\n" !ans
end

include Solution.Make(M)
