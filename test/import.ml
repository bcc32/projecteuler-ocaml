open! Core
module Gen = Quickcheck.Generator
module Obs = Quickcheck.Observer
module Shr = Quickcheck.Shrinker

module Q = Quickcheck.Configure (struct
    include Quickcheck

    let default_trial_count = 2500
  end)

include Euler
include Int.Replace_polymorphic_compare
include Expect_test_helpers_kernel
