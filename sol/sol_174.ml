open! Core

module M = struct
  let problem = `Number 174

  (* TODO think about this as a^2 - b^2 = (a + b)(a - b) *)

  let main () =
    let counts = Int.Table.create () in
    for i = 1 to 500_000 do
      let rec loop j =
        if j < 1
        then ()
        else
          let tiles = i * i - j * j in
          Hashtbl.incr counts tiles;
          loop (j - 2)
      in
      loop (i - 2)
    done;
    Debug.eprint "hi";
    printf !"%{sexp: int Int.Table.t}\n" counts
  ;;
end
include Euler.Solution.Make(M)
