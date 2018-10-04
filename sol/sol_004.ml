open! Core
open! Import

module M = struct
  let problem = `Number 4

  let main () =
    let ans = ref 0 in
    for i = 100 to 999 do
      for j = 100 to 999 do
        let digits = i * j |> Number_theory.Int.digits_of_int in
        if Util.is_palindrome ~equal:Int.equal digits && i * j > !ans then ans := i * j
      done
    done;
    printf "%d\n" !ans
  ;;
end

include Solution.Make (M)
