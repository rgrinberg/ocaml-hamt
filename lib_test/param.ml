(*
   Originally written by Gabriel Scherer
*)

module type Assoc = sig
  type key = int
  type 'a t
  val empty : 'a t
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
end

let mem x t = Array.fold_left (fun b y -> b || x = y) false t

module Config = (val
                  if mem "-32" Sys.argv
                  then (module Hamt.StdConfig32)
                  else (module Hamt.StdConfig)
                  : Hamt.CONFIG)

module AssocHamt =
  Hamt.Make (Config)
    (struct type t = int let hash = Hashtbl.hash end)

module AssocMap =
  Map.Make
    (struct type t = int let compare = compare end)

let implem, nbiter, should_test_add, should_test_find =
  let format = "implem:(hamt|map) nbiter:[0-9]* options:(add|find)*" in
  let error fmt =
    Printf.kfprintf (fun _ -> exit 1) stderr
      ("invalid command line (expected format %S)\n" ^^ fmt) format in
  match Array.to_list Sys.argv with
  | _progname :: hamt :: nbiter :: options ->
    let implem = match hamt with
      | "hamt" -> (module AssocHamt : Assoc)
      | "map" -> (module AssocMap : Assoc)
      | other -> error "invalid impl:%S\n" other
    in
    let nbiter =
      try int_of_string nbiter with _ -> error "invalid nbiter:%S\n" nbiter in
    let test_add = List.exists ((=) "add") options in
    let test_find = List.exists ((=) "find") options in
    let () =
      List.iter
        (fun e -> if e <> "add" && e <> "find" then
            error "invalid option %S\n" e)
        options in
    implem, nbiter, test_add, test_find
  | _ -> error ""

module M = (val implem)


(* Compute the random input separately first, instead of doing it
   during each iteration, to avoid having Random.int taking an
   important amount of time *)
let rec random_input limit n acc =
  if n = 0 then acc
  else random_input limit (n - 1) (Random.int limit :: acc)

let rec test_add t = function
  | [] -> t
  | i::input ->
    test_add (M.add i () t) input

let rec test_find t = function
  | [] -> ()
  | i::input ->
    begin try M.find i t with _ -> () end;
    test_find t input

let () =
  let limit =
    let max_limit = 1 lsl 30 - 1 in
    if float nbiter < sqrt (float max_limit)
    then nbiter * nbiter
    else max_limit in
  let input = random_input limit nbiter [] in
  let nb_add = 10 in
  let nb_read = 50 in

  let hamt = test_add M.empty input in
  if should_test_add then begin
    for i = 0 to nb_add do
      ignore (test_add hamt input)
    done;
  end;
  if should_test_find then begin
    for i = 0 to nb_read / 2 do
      test_find hamt input
    done;
    let other_input = random_input limit nbiter [] in
    for i = 0 to nb_read / 2 do
      test_find hamt other_input
    done
  end
