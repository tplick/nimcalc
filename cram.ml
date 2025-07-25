
open Nimcalc

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
    in faux_shuffle z

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


let c_would_split game =
    if not (should_try_to_split game) then 0 else

    let square = first_square_on_board game
    in
    let region = (pick_off_region game square (c_empty_game game.height game.width))
    in
    let part  = count_squares_on_board region and
        whole = count_squares_on_board game
    in min part (whole - part)


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
    in
    square (r-cr) + square (c-cc)


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

let c_sorted_options game =
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
    List.map (fun (_, x) -> x) w



let cram_db = Bytes.make (1 lsl 24) (char_of_int 255)

let shift_game_to_left game =
    let squares = all_squares_on_board game in
    let c_min = List.fold_left (fun acc (_, c) -> min acc c) 1000 squares and
        c_max = List.fold_left (fun acc (_, c) -> max acc c) 0 squares
    in
    for r = 0 to game.height - 1 do
        game.board.(r) <- game.board.(r) lsr c_min
    done;
    c_max - c_min + 1

let make_code_from_board game width =
    let code = ref 0 in
    for r = 0 to game.height - 1 do
        code := !code lsl width;
        code := !code + game.board.(r)
    done;
    !code

let report_board_from_table board rows cols nimval =
    let game = {board = board; height = rows; width = cols; is_new = false; last_move = None} in
    let new_width = shift_game_to_left game in
    if new_width <= 6
        then (let code = make_code_from_board game 6
              in Printf.printf "%d: \t%d\n" code nimval;
              Bytes.set cram_db code (char_of_int nimval));
    new_width <= 6

let report_nimval_table table0 rows cols =
    let count = ref 0 in
    match !table0 with
        | None -> ()
        | Some table ->
            Array.iter
                (fun entry ->
                    match entry with
                        | None -> ()
                        | Some (_, k, v) -> (if report_board_from_table k rows cols v
                                                then incr count))
                table;
    Printf.printf "Reported %d entries from nim-value table.\n" !count;

    let out = open_out "cram_generated.db" in
    Printf.fprintf out "%s" (Bytes.to_string cram_db);
    close_out out

let cram_nimber_of_game a b =
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1
                then nonzero_nimber_of_game
                else nimber_of_game
    in  fn (c_new_game a b) c_sorted_options c_split (fun x -> x.board)

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

    Printf.printf "All cram tests passed!\n";
    exit 0


let _ =
    if Sys.argv.(1) = "test" then run_tests ();

    let a = int_of_string Sys.argv.(1) and
        b = int_of_string Sys.argv.(2) and
        nimval_tts = new_table_list 1 []
    in
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1
                then nonzero_nimber_of_game_with_nimval_tts
                else nimber_of_game_with_nimval_tts
    in
    let (nimber, time) = with_time
            (fun () -> fn (c_new_game a b) c_sorted_options c_split (fun x -> x.board) nimval_tts)
    in
    Printf.printf "%d x %d: %d  (%.2f sec, %d positions, %d HT hits, %d splits)\n%!"
        a b nimber
        time
        !call_counter
        !hit_counter
        !split_counter;
    report_nimval_table (List.hd nimval_tts) a b

