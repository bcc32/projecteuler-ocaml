open! Core
open! Import

module M = struct
  let problem = Number 17

  let how_many_letters n =
    if n > 1000 then ksprintf invalid_arg "n is too big: %d" n;
    if n < 1 then ksprintf invalid_arg "n is too small: %d" n;
    let less_than_twenty = function
      | 0 -> 0
      | 1 -> String.length "one"
      | 2 -> String.length "two"
      | 3 -> String.length "three"
      | 4 -> String.length "four"
      | 5 -> String.length "five"
      | 6 -> String.length "six"
      | 7 -> String.length "seven"
      | 8 -> String.length "eight"
      | 9 -> String.length "nine"
      | 10 -> String.length "ten"
      | 11 -> String.length "eleven"
      | 12 -> String.length "twelve"
      | 13 -> String.length "thirteen"
      | 14 -> String.length "fourteen"
      | 15 -> String.length "fifteen"
      | 16 -> String.length "sixteen"
      | 17 -> String.length "seventeen"
      | 18 -> String.length "eighteen"
      | 19 -> String.length "nineteen"
      | _ -> assert false
    in
    let last_two_digits n =
      match n / 10 with
      | 0 | 1 -> less_than_twenty n
      | 2 -> less_than_twenty (n % 10) + String.length "twenty"
      | 3 -> less_than_twenty (n % 10) + String.length "thirty"
      | 4 -> less_than_twenty (n % 10) + String.length "forty"
      | 5 -> less_than_twenty (n % 10) + String.length "fifty"
      | 6 -> less_than_twenty (n % 10) + String.length "sixty"
      | 7 -> less_than_twenty (n % 10) + String.length "seventy"
      | 8 -> less_than_twenty (n % 10) + String.length "eighty"
      | 9 -> less_than_twenty (n % 10) + String.length "ninety"
      | _ -> assert false
    in
    let hundred n =
      match n / 100 with
      | 0 -> last_two_digits n
      | (1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9) as h ->
        last_two_digits (n % 100)
        + less_than_twenty h
        + String.length "hundred"
        + if n % 100 = 0 then 0 else String.length "and"
      | _ -> assert false
    in
    let thousand n =
      hundred (n % 1000)
      + if n = 1000 then String.length "one" + String.length "thousand" else 0
    in
    thousand n
  ;;

  let main () =
    Sequence.range 1 1000 ~stop:`inclusive
    |> Sequence.sum (module Int) ~f:how_many_letters
    |> printf "%d\n"
  ;;
end

include Solution.Make (M)
