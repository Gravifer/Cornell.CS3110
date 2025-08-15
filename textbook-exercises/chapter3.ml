(* ::Package:: *)
(*** CS3110 - Exercises ***)

(* ::Title:: *)
(* CS3110 - Chapter 3 *)

(* ::Author:: *)
(* Author: Gravifer *)

type ('a, 'b) tree =
  | Leaf
  | Node of ('a * 'b) * ('a , 'b) tree * ('a , 'b) tree

(* ::Section:: *)
(* exercise: is_bst *)
let rec is_bst tree =
  let rec aux node min_val max_val =
    match node with
    | Leaf -> true
    | Node ((key, _), left, right) ->
        key >= min_val && key <= max_val &&
        aux left min_val key && aux right key max_val
  in
  aux tree min_int max_int
