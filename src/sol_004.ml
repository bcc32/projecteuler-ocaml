open Core.Std

module M = struct
  let problem_number = 4

  let is_palindrome s =
    s = String.rev s

  let main () =
    let ans = ref 0 in
    for i = 100 to 999 do
      for j = 100 to 999 do
        if is_palindrome (Int.to_string (i * j)) && i * j > !ans
        then ans := i * j
      done
    done;
    printf "%d\n" !ans
end

include Solution.Make(M)
