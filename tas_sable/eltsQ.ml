(* Crée EltsI.ORDERED_AND_OPERATIONAL sur les rationnels *)
(* Copié et modifié depuis elts.ml *)

open EltsI
open Matrix


module Rationnal : EltsI.ORDERED_AND_OPERATIONAL with type t = Q.t =
struct

  exception NonElt

  type t = Q.t

  (* Need an epsilon to compare floats. We define the precion here *)
  let epsilon = Q.of_ints 1 1000000

  let zero = Q.zero

  let one = Q.one

  (* We compare based on epsilon. We first find the distance
   * between the two numbers and divide my the maximum to see
   * if that's below epsilon. *)
  let compare a b =
    let a', b' = Q.abs a, Q.abs b in
    if a' < epsilon && b' < epsilon then
      Order.Equal
    else
      let diff = Q.div (Q.sub a b) (max a' b') in
      if Q.abs diff < epsilon then Order.Equal
      else if a < b then Order.Less
      else if a > b then Order.Greater
      else
        raise (Failure "Error in compare.")

  let to_string = Q.to_string

  let from_string (x: string) =
    try
      Q.of_string x
    with
    | Failure (_) -> raise NonElt

  let add = Q.add

  let subtract = Q.sub

  let multiply = Q.mul

  let divide = Q.div

(*   let print a = print_string ((to_string a) ^ "\n") *)
  let print (a: Q.t) =
    let p = Q.to_string a in
    if String.length p < 2 then print_string " " else ();
    print_string p

  let generate () = Q.of_int 3

  let generate_gt a () = Q.add a (Q.of_int 1)

  let generate_lt a () = Q.sub a (Q.of_int 1)

  let generate_between a b () =
    if a > b then None else Some(Q.div (Q.add a b) (Q.of_int 2))

  let generate_x x () = Q.of_float x

  let generate_random bound () =
(*     let x = Random.float bound in *)
(*     x -. mod_float x epsilon *)
    let bound = int_of_float bound in
    Q.of_ints (Random.int bound) (Random.int bound)

  (************************ TESTS ********************************)

  let rec test_compare (times: int) : unit =
    let random t = Q.of_int (Random.int t - Random.int t) in
    if times = 0 then ()
    else
      let x, y = random times, random times in
      match compare x y with
      | Order.Equal -> assert(x = y)
      | Order.Greater -> assert(x > y)
      | Order.Less -> assert(x < y)


  (* Honestly, probably don't need to test the other functions *)
  let run_tests (times: int) : unit =
    test_compare times ;
    ()
end

module RationnalMatrix = MakeMatrix(Rationnal)
