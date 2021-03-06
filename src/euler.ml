(** This module replaces the [Memo] module from [Core_kernel]. *)
module Memo = Memo

open! Core
open! Import
module Algebra = Algebra
module Bitset = Bitset
module Composition_infix = Composition_infix
module Distribution = Distribution
module Geometry = Geometry
module Numerics = Numerics
module Number_theory = Number_theory
module Poker = Poker
module Sequences = Sequences
include Composition_infix.Export
include Interfaces
