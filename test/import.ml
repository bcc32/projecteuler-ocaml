open! Core

module Q = Quickcheck.Configure (struct
    include Quickcheck

    let default_trial_count = 2500
  end)

include Euler
include Int.Replace_polymorphic_compare
include Expect_test_helpers_kernel
