open Printf

let () = Random.self_init ()

module IntMap = Map.Make (struct type t = int let compare = compare end)

let n = int_of_string (Sys.argv.(1))

let random_int bound =
  Int64.to_int (Random.int64 (Int64.of_int bound))

let random min max = 
  try random_int (max - min) + min
  with Invalid_argument "Random.int64" -> min

let generate =
  let rec generate acc n =
    if n = 0 then acc
    else generate (random_int Pervasives.max_int :: acc) (pred n) 
  in generate []

let generate_different li =
  let rec aux acc = function
    | [] -> acc
    | [x] -> random x Pervasives.max_int :: acc
    | x :: y :: zs -> aux (random x y :: acc) (y :: zs)
  in aux [] (List.sort compare li)

(*
   let () = printf "Generating %d random integers\n%!" n
   let presents = List.sort compare (generate n)

   (* 
   presents and absents might share elements, but it is highly unlikely 
   and should have negligeable effect if so
 *)
   let () = printf "Generating %d other random integers\n%!" n
   let absents = List.sort compare (generate_different presents)
*)

(*
   let () = printf "\nInserting elements in a Hamt\n%!"
   let before = Sys.time ()
   let hamt = fill_structure Hamt.add Hamt.empty presents
   let () = printf "%f\n%!" (Sys.time () -. before)


   let () = printf "\nReading present elements in a Hamt\n%!"
   let before = Sys.time ()
   let () = List.iter (fun x -> ignore (Hamt.find x hamt)) presents
   let () = printf "%f\n%!" (Sys.time () -. before)

   let () = printf "\nReading absent elements in a Hamt\n%!"
   let before = Sys.time ()
   let () = List.iter (fun x -> try ignore (Hamt.find x hamt) with Not_found -> ()) absents
   let () = printf "%f\n%!" (Sys.time () -. before)
*)

let rec fill n acc =
  if n = 0 then acc
  else fill (pred n) (Hamt.add (random_int Pervasives.max_int) n acc)

let () = printf "\nInserting elements in a IntMap\n%!"
let before = Sys.time ()
let map = fill n Hamt.empty
let () = printf "%f\n%!" (Sys.time () -. before)

let () = print_int (Hamt.cardinal map)

(*
   let () = printf "\nReading present elements in a IntMap\n%!"
   let before = Sys.time ()
   let () = List.iter (fun x -> ignore (IntMap.find x map)) presents
   let () = printf "%f\n%!" (Sys.time () -. before)

   let () = printf "\nReading absent elements in a IntMap\n%!"
   let before = Sys.time ()
   let () = List.iter (fun x -> try ignore (IntMap.find x map) with Not_found -> ()) absents
   let () = printf "%f\n%!" (Sys.time () -. before)

   let () = printf "\nDeleting elements in a IntMap\n%!"
   let before = Sys.time ()
   let _ = List.fold_left (fun acc x -> IntMap.remove x acc) map presents
   let () = printf "%f\n%!" (Sys.time () -. before)

*)
