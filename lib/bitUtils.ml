let sk5 = 0x55555555
let sk3 = 0x33333333
let skf0 = 0xf0f0f0f
let skff = 0xff00ff

(*
  Given a bitmap, counts the number of one-bits
*)
let ctpop map =
  let map = map - (map lsr 1) land sk5 in
  let map = map land sk3 + (map lsr 2) land sk3 in
  let map = map land skf0 + (map lsr 4) land skf0
  in let map = map + map lsr 8
     in (map + map lsr 16) land 0x3f

(* 
   Given a bitmap and a subhash, returns the index into the list 
*)
let from_bitmap bitmap sub_hash =
  let mask = pred (1 lsl sub_hash) in
  ctpop (bitmap land mask)

(*
  Given a bitmap, returns the list of indices where a one-bit is present
*)
let bitmap_to_indices bitmap =
  let rec loop ix bitmap =
    if bitmap = 0 then []
    else if ix = 32 then []
    else let r = loop (succ ix) (bitmap asr 1)
         in if bitmap land 1 = 0 then r else ix :: r
  in loop 0 bitmap

(*
  Given a list of indices, returns the bitmap with one-bits at these indices
*)
let rec indices_to_bitmap =
  List.fold_left
    (fun x i -> x lor 1 lsl i) 0

(*
  Given a bitmap and an integer, returns the boolean corresponding to
  the n-th bit (0 is the weakest) of the bitmap
*)
let nth_bit_set bitmap n =
  (bitmap asr n) land 1 = 1
