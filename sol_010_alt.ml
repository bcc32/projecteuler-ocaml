open Core.Std

module M = struct
  let problem_number = 10

  type 'a array_list = {
    capacity: int;
    mutable count: int;
    mutable contents: 'a array;
  }

  exception Result of int
  exception NoResult

  let rec division_test n al =
    for i = 0 to al.count - 1 do
      match n with
      | 2 -> raise NoResult
      | _ when n mod al.contents.(i) = 0 ->
        raise (Result (division_test (n + 2) al))
      | _ when (Int.pow al.contents.(i) 2) > n -> raise (Result n)
      | _ -> ()
    done;
    raise NoResult

  let extend al =
    if al.count = al.capacity then
      failwith "no more"
    else begin
      let last = al.contents.(al.count - 1) in
      let next = try division_test (last + 2) al with Result(n) -> n in
      al.contents.(al.count) <- next;
      al.count <- al.count + 1;
    end

  let main () =
    let al =
      { capacity = 200000
      ; count    = 2
      ; contents = Array.create 0 ~len:200000
      }
    in
    al.contents.(0) <- 2;
    al.contents.(1) <- 3;
    while al.contents.(al.count - 1) < 2000000 do
      extend al;
    done;
    print_int ((Array.fold ~f:(+) ~init:0 al.contents) - al.contents.(al.count - 1));
    print_newline ();

end

include Solution.Make(M)
