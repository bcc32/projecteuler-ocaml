open! Core
open! Import

module Export : sig
  (** When true, enable debugging and progress printing in various places in
      solutions.

      Enabled by setting the [EULER_DEBUG] environment variable to a non-empty
      string.  *)
  val debug : bool
end
