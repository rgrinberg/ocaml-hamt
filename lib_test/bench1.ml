open Core.Std
open Core_bench.Std

let max_rand = 1_000_000

let () = Random.self_init ()

module IntMap =
  Caml.Map.Make
    (struct
      type t = int
      let compare = compare
    end)

let rec fill n acc ~f =
  match n with
  | 0 -> acc
  | _ -> fill (pred n) (f (Random.int max_rand) n acc) ~f

let () =
  let args = [1;10;100;1000;10000] in
  Command.run @@
    Bench.make_command [
        Bench.Test.create_indexed
          ~name:"IntMap.add" ~args
          (fun i ->
           Staged.stage @@
             fun () ->
             ignore @@ fill i IntMap.empty ~f:IntMap.add
          );
        Bench.Test.create_indexed
          ~name:"HashTbl.add" ~args
          (fun i ->
           Staged.stage @@
             fun () ->
             ignore @@ fill i (Int.Table.create ())
                            ~f:(fun k v t -> Hashtbl.replace t ~key:k ~data:v; t)
          );
        Bench.Test.create_indexed
          ~name:"Hamt.Int.add" ~args
          (fun i ->
           Staged.stage @@
             fun () ->
             ignore @@ fill i (Hamt.Int.empty)
                            ~f:(Hamt.Int.add)
          )
      ]
