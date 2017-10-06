open Core

module M = struct
  let problem_number = failwith "unimplemented"

  let main () =
    failwith "unimplemented"
end

include Euler.Solution.Make(M)
