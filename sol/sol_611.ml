open! Core
open! Import

module M = struct
  let problem = `Number 611

  let f n =
    let opened = Int.Hash_set.create () in
    let rec loop1 i ii =
      if ii > n
      then ()
      else (
        let rec loop2 j jj =
          if ii + jj > n
          then ()
          else (
            (* toggle set membership *)
            (try Hash_set.strict_add_exn opened (ii + jj) with
             | _ -> Hash_set.strict_remove_exn opened (ii + jj));
            let j = j + 1 in
            loop2 j (j * j))
        in
        let i = i + 1 in
        loop2 i (i * i);
        loop1 i (i * i))
    in
    loop1 1 1;
    Hash_set.length opened
  ;;

  let main () = printf "%d\n" @@ f 1_000_000_000_000
end

include Solution.Make (M)
