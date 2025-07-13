
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

let rec round_up n =
    if n = 0
        then 0
        else n lor round_up (n lsr 1)

let table_mask =
    try
        let n = int_of_string (Unix.getenv "NIMCALC_TABLE_SIZE")
        in let mask = round_up (n - 1)
        in if n <= 0 then raise (Invalid_argument "");
        if verbose then Printf.printf "Using table size of %d.\n" (mask + 1);
        mask
    with e ->
        if verbose then Printf.printf "Using default table size of 8192.\n";
        8191


let null_splitter _ = None

let call_counter = ref 0
let hit_counter = ref 0
let split_counter = ref 0

let reset_counters () =
    call_counter := 0;
    hit_counter := 0;
    split_counter := 0

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



let new_table () =
    Array.make (table_mask + 1) None

let get_or_create_tt tt =
    match !tt with
        | Some x -> x
        | None -> let new_tt = new_table ()
                  in tt := Some new_tt;
                     new_tt

let add_to_table tt game value =
    let h = Hashtbl.hash_param 256 256 game
    in (get_or_create_tt tt).(h land table_mask) <- Some (h, game, value)

let look_up_in_table tt game =
    let h = Hashtbl.hash_param 256 256 game
    in match (get_or_create_tt tt).(h land table_mask) with
        | Some (h2, k, v) when h = h2 && k = game ->
            incr hit_counter; Some v
        | _ -> None

let rec new_table_list n acc =
    if n == 0
        then acc
        else new_table_list (n-1) (ref None :: acc)


let rec
is_game_a_win (Game (game, nimheap)) optgen splitter tts hasher nimval_tts =
    incr call_counter;

    let options = optgen (Game (game, nimheap))
    in

    let compute () = (match splitter game with
        | None -> List.exists (fun opt -> is_game_a_loss opt optgen splitter (List.tl tts) hasher (nimval_tts)) options
        | Some (g, h) ->
            incr split_counter;
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
    let tt = new_table_list 100 [] in
    List.iter
        (fun table ->
            for i = 0 to candidate - 1 do
                add_to_table table (hasher game, i) true
            done)
        tt;
    if is_game_a_loss_top (Game (game, candidate)) optgen splitter tt hasher nimval_tts
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

