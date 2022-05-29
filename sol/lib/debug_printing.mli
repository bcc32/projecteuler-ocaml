open! Core
open! Import

module Export : sig
  (** When true, enable debugging and progress printing in various places in
      solutions.

      Enabled by setting the [EULER_DEBUG] environment variable to a non-empty
      string.  *)
  val debug : bool

  (** When [debug] is true, [debug_timing here f x] prints the elapsed time taken to
      evaluate (f x) and returns the result. *)
  val debug_timing : Source_code_position.t -> string -> (unit -> 'a) -> 'a
end
