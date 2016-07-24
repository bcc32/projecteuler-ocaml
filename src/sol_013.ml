open Core.Std
open Bignum.Std

module M = struct
  let problem_number = 13

  let path = "data/013.txt"
  let prefix_length = 10

  let numbers =
    lazy (
      In_channel.with_file path ~f:(fun chan ->
        In_channel.input_lines chan
        |> List.map ~f:Bignum.of_string
      )
    )

  let main () =
    let numbers = force numbers in
    List.fold numbers ~init:Bignum.zero ~f:Bignum.(+)
    |> Bignum.to_string
    |> String.subo ~len:prefix_length
    |> printf "%s\n"
end

include Solution.Make(M)
