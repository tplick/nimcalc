
open Nimcalc

let no_db =
    try
        ignore (Unix.getenv "NIMCALC_NO_DB");
        true
    with e ->
        false


module SqSet = Set.Make (struct
    type t = int * int

    let compare (a, b) (c, d) =
        (a - c) lsl 10 + (b - d)
end)

let over list fn = List.iter fn list



type cram_game = {board: int array; is_new: bool; last_move: ((int * int) * (int * int)) option;
            height: int; width: int}

let c_new_game a b =
    let board = Array.make a (1 lsl b - 1)
    in {board = board; is_new = true; last_move = None; height = a; width = b}

let c_empty_game a b =
    let board = Array.make a 0
    in {board = board; is_new = false; last_move = None; height = a; width = b}

let can_move_horiz game (a, b) =
    if b + 1 >= game.width
        then false
        else

    let mask = 3 lsl b and
        row = game.board.(a)
    in row land mask = mask

let can_move_vert  game (a, b) =
    if a + 1 >= game.height
        then false
        else

    let mask = 1 lsl b
    in game.board.(a) land game.board.(a+1) land mask = mask

let board_without game (a, b) (c, d) =
    let new_board = Array.copy game.board
    in  new_board.(a) <- new_board.(a) land lnot (1 lsl b);
        new_board.(c) <- new_board.(c) land lnot (1 lsl d);
        new_board

let c_after_horiz_move game (a, b) =
  {game with
     board = board_without game (a, b) (a, b+1);
     is_new = false;
     last_move = Some ((a, b), (a, b+1))}

let c_after_vert_move game (a, b) =
  {game with
     board = board_without game (a, b) (a+1, b);
     is_new = false;
     last_move = Some ((a, b), (a+1, b))}


let get_last_move opt =
    match opt.last_move with
        | Some x -> x
        | None -> ((0, 0), (0, 0))

let rec split_list_alternating x acc1 acc2 =
    match x with
        | [] -> acc1, acc2
        | [y] -> y :: acc1, acc2
        | y :: z :: rest -> split_list_alternating rest (y :: acc1) (z :: acc2)

let rec faux_shuffle = function
    | [] -> []
    | [x] -> [x]
    | x -> let y, z = split_list_alternating x [] []
           in (faux_shuffle y) @ (faux_shuffle z)

let all_squares_on_board game =
    let res = ref [] in
    for r = 0 to game.height - 1 do
        let row = ref game.board.(r) in
        let c = ref 0 in
        while !row > 0 do
            if !row land 1 > 0
                then res := (r, !c) :: !res;
            incr c;
            row := !row lsr 1
        done
    done;
    !res

let swap arr i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp

let invert_last_move game =
    match game.last_move with
        | None -> None
        | Some ((a, b), (c, d)) ->
            Some ((game.height - 1 - a, b), (game.height - 1 - c, d))

let flip_game_3 game =
    let new_game = {game with board = Array.copy game.board; last_move = invert_last_move game}
    in  swap new_game.board 0 2;
    new_game

let flip_game_4 game =
    let new_game = {game with board = Array.copy game.board; last_move = invert_last_move game}
    in  swap new_game.board 0 3;
        swap new_game.board 1 2;
    new_game

let try_to_flip game =
    if game.height == 3 && game.board.(0) > game.board.(2)
        then flip_game_3 game
        else
    if game.height == 4 && game.board.(1) > game.board.(2)
        then flip_game_4 game
        else
    game

let c_options_for_game game =
    let full_square_list = all_squares_on_board game
    in
    let square_list = if game.is_new
        then List.filter (fun (a, b) -> a <= (game.height - 1) / 2 && b <= (game.width - 1) / 2) full_square_list
        else full_square_list
    in
    let horiz_moves = List.filter (can_move_horiz game) square_list
    and vert_moves = List.filter (can_move_vert game) square_list
    in
    let z = (List.map (c_after_horiz_move game) horiz_moves) @ (List.map (c_after_vert_move game) vert_moves)
    in faux_shuffle (List.map try_to_flip z)

let is_square_on_board (r, c) game =
    r >= 0 && c >= 0 && r < game.height && c < game.width &&
    game.board.(r) land (1 lsl c) > 0

let add_square_to_board (r, c) game =
    game.board.(r) <- game.board.(r) lor (1 lsl c)

let rec pick_off_region game square set =
    if is_square_on_board square set
        then set
        else 

   (add_square_to_board square set;
    let (a, b) = square in
    over [(a, b+1); (a, b-1); (a+1, b); (a-1, b)] (fun sq ->
        if is_square_on_board sq game
            then ignore (pick_off_region game sq set)
    );
    set)

let does_have_at_most_two_neighbors (a, b) game =
    let count = ref 0
    in
    over [(a, b+1); (a, b-1); (a+1, b); (a-1, b)] (fun sq ->
        if is_square_on_board sq game then incr count);
    !count <= 2


let make_set_from_board game =
    let set = ref SqSet.empty in
    for r = 0 to game.height - 1 do
        let row = game.board.(r) in
        for c = 0 to game.width - 1 do
            if row land (1 lsl c) > 0
                then set := SqSet.add (r, c) !set
        done
    done;
    !set

let does_board_have_at_least game max =
    let count = ref 0 in
    for r = 0 to game.height - 1 do
        let row = ref game.board.(r) in
        while !row > 0 && !count < max do
            row := !row land (!row - 1);
            incr count
        done
    done;
    !count >= max

let should_try_to_split game =
    if true then true else
    match game.last_move with
        | None -> false
        | Some ((a, b), (c, d)) ->
            (does_have_at_most_two_neighbors (a, b) game
          || does_have_at_most_two_neighbors (c, d) game)
          && does_board_have_at_least game 5
          && game.last_move <> None

let rec set_map fn set =
    if SqSet.is_empty set
        then SqSet.empty
        else
            let elt = SqSet.choose set
            in
            SqSet.add (fn elt) (set_map fn (SqSet.remove elt set))

let make_board_from_set game set =
    let arr = Array.make game.height 0 in
    SqSet.iter
        (fun (r, c) ->
            arr.(r) <- arr.(r) lor (1 lsl c))
        set;
    arr

let first_square_on_board game =
    let sq = ref None in
    for r = 0 to game.height - 1 do
        if !sq == None then
           (let row = game.board.(r) in
            for c = 0 to game.width - 1 do
                if row land (1 lsl c) > 0
                    then sq := Some (r, c)
            done)
    done;

    match !sq with
        | Some x -> x
        | None -> (0, 0)


let bit_count_array = [| 0; 1; 1; 2; 1; 2; 2; 3 |]
let count_squares_on_board game =
    let count = ref 0 in
    for r = 0 to game.height - 1 do
        let row = ref game.board.(r) in
        while !row > 0 do
            count := !count + bit_count_array.(!row land 7);
            row := !row lsr 3
        done
    done;
    !count

let difference_of_boards game region =
    let diff = Array.make game.height 0
    in
    for r = 0 to game.height - 1 do
        diff.(r) <- game.board.(r) land lnot region.board.(r)
    done;

   {board = diff;
    is_new = false;
    width = game.width;
    height = game.height;
    last_move = None}

let c_split game =
    if not (should_try_to_split game) then None else

    let square = first_square_on_board game
    in
    let region = (pick_off_region game square (c_empty_game game.height game.width))
    in
    let region_size, game_size = count_squares_on_board region, count_squares_on_board game
    in
    if region_size == game_size
        then None
        else let remains = difference_of_boards game region
             in if region_size <= game_size - region_size
                then Some ({game with board = region.board;  is_new = false},
                           {game with board = remains.board; is_new = false})
                else Some ({game with board = remains.board; is_new = false},
                           {game with board = region.board;  is_new = false})

let number_of_available_center_cells game =
    let count = ref 0 and c = game.width lsr 1 in
    for r = 0 to game.height - 1 do
        if game.board.(r) land (1 lsl c) > 0
            then incr count
    done;
    !count

let number_of_available_cells_in_column game c =
    let count = ref 0 in
    for r = 0 to game.height - 1 do
        if game.board.(r) land (1 lsl c) > 0
            then incr count
    done;
    !count

let c_would_split game =
    if not (should_try_to_split game) then 0 else

    let square = first_square_on_board game
    in
    let region = (pick_off_region game square (c_empty_game game.height game.width))
    in
    let part  = count_squares_on_board region and
        whole = count_squares_on_board game
    in let v = min part (whole - part)
    in if v <= 1
        then (if number_of_available_center_cells game == 1 ||
                 number_of_available_cells_in_column game 0 == 0
              then 1 else 0)
        else v * 1000 + (if number_of_available_center_cells game == 1 then 1 else 0)


let calculate_center game =
    let (x_sum, y_sum, count) =
        List.fold_left (fun (a, b, count) (c, d) -> (a + c, b + d, count + 1))
                       (0, 0, 0)
                       (all_squares_on_board game)
    in
    if count = 0
        then (0, 0)
        else (x_sum / count, y_sum / count)

let distance_of_last_move_from_center game =
    let (r, c), _ = get_last_move game
    and (cr, cc) = calculate_center game
    and square x = x * x
    and is_move_vertical = (match get_last_move game with (a, b), (c, d) -> b == d)
    in
    square (r-cr+1) + 5 * square (c-cc) + (if is_move_vertical then 0 else 1)


let pull_to_front fn xs =
    let ys, zs = List.partition fn xs
    in ys @ zs

(*
let is_row_or_column_missing game =
    let (r1, c1), (r2, c2) = get_last_move game
    in
    is_row_missing game r1 || (r1 <> r2 && is_row_missing game r2) ||
    is_column_missing game c1 || (c1 <> c2 && is_column_missing game c2)
*)

let true_width_of_game game =
    let squares = all_squares_on_board game in
    let c_min = List.fold_left (fun acc (_, c) -> min acc c) 1000 squares and
        c_max = List.fold_left (fun acc (_, c) -> max acc c) 0 squares in
    c_max - c_min + 1

let cram_db =
    let directory_name = Filename.dirname Sys.argv.(0) in
    let inn = open_in (directory_name ^ "/cram4by6.db") in
    let s = input_line inn in
    close_in inn;
    s

let make_shifted_game game =
    let squares = all_squares_on_board game in
    let c_min = List.fold_left (fun acc (_, c) -> min acc c) 1000 squares and
        new_game = {game with board = Array.make game.height 0} in
    for r = 0 to game.height - 1 do
        new_game.board.(r) <- game.board.(r) lsr c_min
    done;
    new_game

let make_code_from_board game width =
    let code = ref 0 in
    for r = 0 to game.height - 1 do
        code := !code lsl width;
        code := !code + game.board.(r)
    done;
    if !code land ((1 lsl width) - 1) == 0
        then code := !code lsr width;
    !code

let look_up_game_in_db game =
    if not no_db &&
                (game.height <= 4 || (game.height == 5 && (game.board.(0) == 0 || game.board.(4) == 0))) &&
                true_width_of_game game <= 6
        then (let shifted = make_shifted_game game in
              let code = make_code_from_board shifted 6 in
              let v = int_of_char cram_db.[code] in
              if v < 255 then Some v else None)
        else
            None

let minimum_column row0 =
    let row = ref row0 and c = ref 0 in
    while !row land 1 == 0 do
        row := !row lsr 1;
        incr c
    done;
    !c

let maximum_column row0 =
    let row = ref row0 and c = ref 0 in
    while !row > 1 do
        row := !row lsr 1;
        incr c
    done;
    !c

let is_unbroken_even_row row =
    let c_min = minimum_column row and
        c_max = maximum_column row in
    (c_max - c_min + 1) land 1 == 0 &&
    row == (1 lsl (c_max + 1)) - (1 lsl c_min)

let () =
    assert (minimum_column 6 == 1);
    assert (maximum_column (32 + 8 + 4) == 5);
    assert (is_unbroken_even_row (4 + 8 + 16 + 32));
    assert (not @@ is_unbroken_even_row (4 + 8 + 16));
    assert (not @@ is_unbroken_even_row (4 + 8 + 32 + 64))

let is_even_rectangle game =
    if game.height == 0
        then true
        else
    if game.height land 1 == 1
        then false
        else

    let first_row = game.board.(0) in
    if Array.for_all (fun elt -> elt == first_row) game.board
        then first_row == 0 || is_unbroken_even_row first_row
        else false

let c_sorted_options game =
    match look_up_game_in_db game with
        | Some v -> DirectNimber v
        | None ->
    if is_even_rectangle game
        then DirectNimber 0
        else

    let opts = c_options_for_game game
    in
    let dec = List.map (fun opt -> (distance_of_last_move_from_center opt, opt)) opts
    in
    let s = List.sort (fun (a, b) (c, d) -> a - c) dec
    in
    let y = List.map (fun (a, b) -> b) s
    in
    let z = List.map (fun opt -> (c_would_split opt, opt)) y
    in
    let w = List.sort (fun (a, _) (b, _) -> b - a) z
    in
    let x = List.map (fun (_, x) -> x) w
    in
    OptionList x

let cram_nimber_of_game a b =
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1
                then nonzero_nimber_of_game
                else nimber_of_game
    in  fn (c_new_game a b) c_sorted_options c_split (fun x -> x.board) (fun _ -> ())

let run_test r c target_value =
    let computed_value = cram_nimber_of_game r c in
    if target_value = computed_value
        then  Printf.printf "%d x %d:\t%d\n%!" r c computed_value
        else (Printf.printf "%d x %d:\texpected %d, got %d\n" r c target_value computed_value;
              Printf.printf "Test failed.\n";
              exit 1)


let run_tests () =
    for c = 0 to 12 do
        run_test 2 c (c land 1)
    done;

    let three_results = [0; 1; 1; 0; 1; 1; 4; 1; 3; 1] in
    for c = 0 to 9 do
        run_test 3 c (List.nth three_results c)
    done;

    run_test 4 4 0;
    run_test 4 5 2;
    run_test 4 6 0;
    run_test 5 5 0;
    run_test 8 20 0;

    Printf.printf "All cram tests passed!\n";
    exit 0


let make_game_from_code code0 =
    let game = c_new_game 4 6 and
        code = ref code0 in
    for r = 0 to 3 do
        game.board.(r) <- !code land 63;
        code := !code lsr 6
    done;
    {game with is_new = false}


let rec mex_of_list lst target =
    let ys, zs = List.partition ((==) target) lst in
    match ys with
        | [] -> target
        | _ -> mex_of_list zs (target + 1)

let nimber_of_game_based_on_db game =
    let options = c_options_for_game game in
    let nimber_options = List.map look_up_game_in_db options in
    let nimbers = List.map (fun n -> match n with Some v -> v | None -> invalid_arg "game not found in db") nimber_options in
    mex_of_list nimbers 0

let run_db_tests () =
    for code = 0 to 1 lsl 24 - 1 do
        if cram_db.[code] <> char_of_int 255
            then (let game = make_game_from_code code in
                  let expected_value = int_of_char cram_db.[code] and
                      computed_value = nimber_of_game_based_on_db game
                  in if expected_value = computed_value
                        then (if code mod 10000 = 0
                                  then Printf.printf "  Done %d of %d...\r%!" code (1 lsl 24))
                        else (Printf.printf "Mismatch for code %d: got %d, expected %d.\n"
                                            code computed_value expected_value;
                              Printf.printf "Db tests failed.\n";
                              exit 1))
    done;
    Printf.printf "\nAll db tests passed!\n";
    exit 0


let make_db () =
    let db = Bytes.make (1 lsl 24) (char_of_int 255) in
    for code = 1 lsl 24 - 1 downto 0 do
        let game = make_game_from_code code in
        let computed_value = nimber_of_game game c_sorted_options c_split (fun x -> x.board) (fun _ -> ()) in
        Bytes.set db code (char_of_int computed_value);
        (if code mod 1000 = 0
             then Printf.printf "  %d of %d remaining...    \r%!" code (1 lsl 24))
    done;

    Printf.printf "\nWriting db out to cram_computed.db...\n";
    let out = open_out "cram_computed.db" in
    Printf.fprintf out "%s" (Bytes.to_string db);
    close_out out;

    Printf.printf "Done.\n";
    exit 0


let checksum_db () =
    let expected_digest = "54bcd287471df5d24e7862a956615d3f0a4d9d844d2e7e623491c8e6e4525e7879d199891c3398b7b3c1bf1f560d84e7c61c6962c7a4ea363faeb7968b35efa1" and
        computed_digest = Digest.BLAKE512.to_hex @@ Digest.BLAKE512.string cram_db in
    if expected_digest <> computed_digest
        then (Printf.printf "Error: Cram database cram4by6.db seems to be corrupted.\n";
              Printf.printf "       Expected BLAKE512 digest %s, got %s.\n"
                            expected_digest
                            computed_digest;
              exit 1)

let try_to_make_move game direc rs cs =
    let r = int_of_string rs and c = int_of_string cs
    in
    match direc with
        | "h" | "H" -> c_after_horiz_move game (r, c)
        | "v" | "V" -> c_after_vert_move game (r, c)
        | _ -> invalid_arg "direction must be h or v"

let print_game game =
    for r = 0 to game.height - 1 do
        for c = 0 to game.width - 1 do
            Printf.printf "%s" (if is_square_on_board (r, c) game then "O" else " ");
        done;
        Printf.printf "\n"
    done

let c_report_last_move (opt, nimheap) =
    match opt.last_move with
        | Some ((a, b), (c, d)) -> Printf.eprintf "  ((%d, %d), (%d, %d)) + %d\n%!" a b c d nimheap
        | None -> Printf.eprintf "  None + %d   \n%!" nimheap

let _ =
    checksum_db ();

    if Sys.argv.(1) = "test" then run_tests ();
    if Sys.argv.(1) = "testdb" then run_db_tests ();
    if Sys.argv.(1) = "makedb" then make_db ();

    let a = int_of_string Sys.argv.(1) and b = int_of_string Sys.argv.(2)
    in
    let game = c_new_game a b
    in
    let game_after_move =
        try
            try_to_make_move game Sys.argv.(3) Sys.argv.(4) Sys.argv.(5)
        with _ ->
            game
    in
    if game != game_after_move
        then print_game game_after_move;
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1 && game == game_after_move
                then nonzero_nimber_of_game
                else nimber_of_game
    in let (nimber, time) = with_time
            (fun () -> fn game_after_move c_sorted_options c_split (fun x -> x.board) c_report_last_move)
    in
    Printf.printf "%d x %d%s: %d  (%.2f sec, %d positions, %d HT hits, %d splits)\n%!"
        a b
        (if game == game_after_move then "" else " alt")
        nimber
        time
        !call_counter
        !hit_counter
        !split_counter

