
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
        let row = game.board.(r) in
        for c = 0 to game.width - 1 do
            if row land (1 lsl c) > 0
                then res := (r, c) :: !res
        done
    done;
    List.rev !res

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

let rec pick_off_region game square set_ref =
    if SqSet.mem square !set_ref
        then set_ref
        else 

   (set_ref := SqSet.add square !set_ref;
    let (a, b) = square in
    over [(a, b+1); (a, b-1); (a+1, b); (a-1, b)] (fun sq ->
        if is_square_on_board sq game
            then ignore (pick_off_region game sq set_ref)
    );
    set_ref)

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

let c_split game =
    if not (should_try_to_split game) then None else

    let board_set = make_set_from_board game
    in
    let square = SqSet.choose board_set
    in
    let region = !(pick_off_region game square (ref SqSet.empty))
    in
    let region_size, game_size = SqSet.cardinal region, SqSet.cardinal board_set
    in
    if region_size == game_size
        then None
        else let remains = SqSet.diff board_set region
             in if region_size <= game_size - region_size
                then Some ({game with board = make_board_from_set game region},
                           {game with board = make_board_from_set game remains})
                else Some ({game with board = make_board_from_set game remains},
                           {game with board = make_board_from_set game region})


let c_would_split game =
    if not (should_try_to_split game) then false else

    let board_set = make_set_from_board game
    in
    let square = SqSet.choose board_set
    in
    let region = !(pick_off_region game square (ref SqSet.empty))
    in
    if SqSet.cardinal region == SqSet.cardinal board_set
        then false
        else true


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
    pull_to_front (fun opt -> c_would_split opt) y


let _ =
    let a = int_of_string Sys.argv.(1) and b = int_of_string Sys.argv.(2)
    in
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1
                then nonzero_nimber_of_game
                else nimber_of_game
    in
    let (nimber, time) = with_time
            (fun () -> fn (c_new_game a b) c_sorted_options c_split (fun x -> x.board))
    in
    Printf.printf "%d x %d: %d  (%.2f sec, %d positions, %d HT hits, %d splits)\n%!"
        a b nimber
        time
        !call_counter
        !hit_counter
        !split_counter

