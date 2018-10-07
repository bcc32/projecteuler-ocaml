open! Core
open! Import

module M = struct
  let problem = Number 166
  let[@inline always] is_in_range x = x >= 0 && x <= 9

  (*
     {v
       a b c d
       e f g h
       i j k l
       m n o p
     v}
  *)

  let main () =
    let count = ref 0 in
    for a = 0 to 9 do
      Debug.eprint_s [%message (a : int)];
      for b = 0 to 9 do
        for c = 0 to 9 do
          for d = 0 to 9 do
            let x = a + b + c + d in
            for e = 0 to 9 do
              for f = 0 to 9 do
                for g = 0 to 9 do
                  let h = x - e - f - g in
                  if is_in_range h
                  then
                    for i = 0 to 9 do
                      for j = 0 to 9 do
                        for k = 0 to 9 do
                          let l = x - i - j - k in
                          if is_in_range l
                          then (
                            let m = x - a - e - i in
                            if is_in_range m && m + j + g + d = x
                            then (
                              let n = x - b - f - j in
                              if is_in_range n
                              then (
                                let o = x - c - g - k in
                                if is_in_range o
                                then (
                                  let p = x - m - n - o in
                                  if is_in_range p && a + f + k + p = x then incr count))))
                        done
                      done
                    done
                done
              done
            done
          done
        done
      done
    done;
    printf "%d\n" !count
  ;;

  (* 7130034
     12s *)
end

include Solution.Make (M)
