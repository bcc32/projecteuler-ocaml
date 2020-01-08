open! Core
open! Import

let%test_module "Compare Bitset with Int.Set" =
  (module (
   struct
     type t = Bitset.t [@@deriving compare, equal, hash, sexp]

     let empty = Bitset.empty
     let is_allowable_element = Bitset.is_allowable_element

     module Elt = struct
       type t = int [@@deriving sexp_of]

       let quickcheck_generator =
         Quickcheck.Generator.small_non_negative_int
         |> Quickcheck.Generator.filter ~f:is_allowable_element
       ;;

       let quickcheck_observer = Int.quickcheck_observer
       let quickcheck_shrinker = Int.quickcheck_shrinker
     end

     let of_list = Bitset.of_list
     let to_set t = t |> Bitset.to_list |> Int.Set.of_list

     module Test = struct
       type nonrec t = t * Int.Set.t [@@deriving sexp_of]

       let quickcheck_generator =
         let open Quickcheck.Let_syntax in
         let%map_open elements = list [%quickcheck.generator: Elt.t] in
         of_list elements, Int.Set.of_list elements
       ;;

       let quickcheck_observer = [%quickcheck.observer: _ * Elt.t Set.t]
       let quickcheck_shrinker = [%quickcheck.shrinker: _ * Elt.t Set.t]
     end

     let mem = Bitset.mem

     let%test_unit "mem" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Elt.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t, set), elt) ->
           [%test_result: bool] (mem t elt) ~expect:(Set.mem set elt))
     ;;

     let add = Bitset.add

     let%test_unit "add" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Elt.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t, set), elt) ->
           [%test_result: Int.Set.t]
             (to_set (add t elt))
             ~expect:(Set.add set elt))
     ;;

     let remove = Bitset.remove

     let%test_unit "remove" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Elt.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t, set), elt) ->
           [%test_result: Int.Set.t]
             (to_set (remove t elt))
             ~expect:(Set.remove set elt))
     ;;

     let union = Bitset.union

     let%test_unit "union" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t1, set1), (t2, set2)) ->
           [%test_result: Int.Set.t]
             (to_set (union t1 t2))
             ~expect:(Set.union set1 set2))
     ;;

     let inter = Bitset.inter

     let%test_unit "inter" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t1, set1), (t2, set2)) ->
           [%test_result: Int.Set.t]
             (to_set (inter t1 t2))
             ~expect:(Set.inter set1 set2))
     ;;

     let diff = Bitset.diff

     let%test_unit "diff" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t * Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun ((t1, set1), (t2, set2)) ->
           [%test_result: Int.Set.t]
             (to_set (diff t1 t2))
             ~expect:(Set.diff set1 set2))
     ;;

     let fold = Bitset.fold

     let%test_unit "fold" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (t, set) ->
           let init = [] in
           let f accum x = x :: accum in
           [%test_result: int list]
             (fold t ~init ~f)
             ~expect:(Set.fold set ~init ~f))
     ;;

     let iter = Bitset.iter

     let%test_unit "iter" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (t, set) ->
           let iter_result t ~iter =
             let results = ref [] in
             iter t ~f:(fun x -> results := x :: !results);
             !results
           in
           [%test_result: int list]
             (iter_result t ~iter)
             ~expect:(iter_result set ~iter:Set.iter))
     ;;

     let length = Bitset.length

     let%test_unit "length" =
       Base_quickcheck.Test.run_exn
         (module struct
           type nonrec t = Test.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (t, set) ->
           [%test_result: int] (length t) ~expect:(Set.length set))
     ;;

     (* Derived from container interface. *)

     let is_empty = Bitset.is_empty
     let fold_result = Bitset.fold_result
     let fold_until = Bitset.fold_until
     let exists = Bitset.exists
     let for_all = Bitset.for_all
     let count = Bitset.count
     let sum = Bitset.sum
     let find = Bitset.find
     let find_map = Bitset.find_map
     let to_list = Bitset.to_list
     let to_array = Bitset.to_array
     let min_elt = Bitset.min_elt
     let max_elt = Bitset.max_elt
   end :
     module type of struct
     include Bitset
   end))
;;
