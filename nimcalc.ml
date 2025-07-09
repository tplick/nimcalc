
let verbose =
    try
        ignore (Unix.getenv "NIMCALC_VERBOSE");
        true
    with e ->
        false

let nosplit =
    try
        ignore (Unix.getenv "NIMCALC_NOSPLIT");
        true
    with e ->
        false

let cpus =
    try
        int_of_string (Unix.getenv "NIMCALC_PROCS")
    with e ->
        1

let null_splitter _ = None

let call_counter = ref 0
let hit_counter = ref 0


let push_onto elt list_ref =
    list_ref := elt :: !list_ref

let shuffle list =
    let arr = Array.of_list list in
    let n = Array.length arr
    in
    for i = 0 to n - 2 do
        let j = i + Random.int (n - i) and swap = arr.(i)
        in
        arr.(i) <- arr.(j); arr.(j) <- swap
    done;
    Array.to_list arr



type 'a game = Game of ('a * int)

let rec options_for_compound optgen topgame =
    match topgame with 
        | Game (game, nimheap) ->
    let options = ref [] and suboptions = optgen game
    in
    List.iter (fun g -> push_onto (Game (g, nimheap)) options) suboptions;
    for i = 0 to (nimheap - 1) do
        push_onto (Game (game, i)) options
    done;
    List.rev !options



type ('a, 'b) tt_table = {hashtable: ('a, 'b) Hashtbl.t; mutable size: int}

let add_to_table tt game value =
    (if Hashtbl.length tt.hashtable >= tt.size
        then Hashtbl.clear tt.hashtable);
    Hashtbl.add tt.hashtable game value

let look_up_in_table tt game =
    match Hashtbl.find_opt tt.hashtable game with
        | (Some v) as x -> (incr hit_counter; x)
        | None -> None

let new_table () =
    {hashtable = Hashtbl.create 100; size = 10000}

let rec new_table_list n acc =
    if n == 0
        then acc
        else new_table_list (n-1) (new_table () :: acc)


let rec
is_game_a_win (Game (game, nimheap)) optgen splitter tts hasher nimval_tts =
    incr call_counter;

    let options = optgen (Game (game, nimheap))
    in

    let compute () = (match splitter game with
        | None -> List.exists (fun opt -> is_game_a_loss opt optgen splitter (List.tl tts) hasher (nimval_tts)) options
        | Some (g, h) ->
            let g_nimber = nimber_of_game' g 0 optgen splitter tts hasher nimval_tts
            in
            is_game_a_win (Game (h, g_nimber lxor nimheap)) optgen splitter tts hasher nimval_tts)
    in
    
    if List.length options < (if splitter == null_splitter then 10 else 5)
        then compute ()
        else

    let hashed_game = hasher game in
    match look_up_in_table (List.hd nimval_tts) (hashed_game) with
        | Some v -> v <> nimheap
        | None ->
    match look_up_in_table (List.hd tts) (hashed_game, nimheap) with
        | Some v -> v
        | None ->
    let value = compute ()
    in add_to_table (List.hd tts) (hashed_game, nimheap) value;
    (if value == false then add_to_table (List.hd nimval_tts) (hashed_game) nimheap);
    value

and is_game_a_loss game optgen splitter tt hasher nimval_tts =
    not (is_game_a_win game optgen splitter tt hasher nimval_tts)

and is_game_a_loss_top game optgen splitter tt hasher nimval_tts =
    let count = List.length (optgen game) and idx = ref 0
    in
    (if verbose then Printf.printf "  There are %d options to try.\n%!" count);
    let options = optgen game
    in
    let result = not (Par.par_list_exists_new (fun opt ->
            (if verbose && cpus = 1 then Printf.printf "  Trying option #%d...\r%!" (!idx+1));
            incr idx;
            is_game_a_loss opt optgen splitter tt hasher nimval_tts) options (List.length options) cpus verbose) in
    (if verbose && cpus = 1 then Printf.printf "\n%!");
    result

and nimber_of_game' game candidate optgen splitter tts hasher nimval_tts =
    if is_game_a_loss (Game (game, candidate)) optgen splitter tts hasher nimval_tts
        then candidate
        else nimber_of_game' game (candidate + 1) optgen splitter tts hasher nimval_tts

and nimber_of_game_top' game candidate optgen splitter hasher nimval_tts =
    (if verbose then Printf.printf "Trying nimber %d...\n%!" candidate);
    if is_game_a_loss_top (Game (game, candidate)) optgen splitter (new_table_list 100 []) hasher nimval_tts
        then candidate
        else nimber_of_game_top' game (candidate + 1) optgen splitter hasher nimval_tts



let nimber_of_game game optgen splitter hasher =
    nimber_of_game_top' game 0 (options_for_compound optgen) (if nosplit then null_splitter else splitter) hasher (new_table_list 100 [])

let nonzero_nimber_of_game game optgen splitter hasher =
    nimber_of_game_top' game 1 (options_for_compound optgen) (if nosplit then null_splitter else splitter) hasher (new_table_list 100 [])



let with_time fn =
    let a = Sys.time () in
    let value = fn () in
    let b = Sys.time () in
    (value, b -. a)

