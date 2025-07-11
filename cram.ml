
open Nimcalc

module SqSet = Set.Make (struct
    type t = int * int

    let compare (a, b) (c, d) =
        (a - c) * 1000 + (b - d)
end)

let over list fn = List.iter fn list



type cram_game = {board: SqSet.t; is_new: bool; last_move: ((int * int) * (int * int)) option;
            height: int; width: int}

let c_new_game a b =
    let set = ref SqSet.empty
    in
    for i = 1 to a do
        for j = 1 to b do
            set := SqSet.add (i, j) !set
        done
    done;
    {board = !set; is_new = true; last_move = None; height = a; width = b}

let can_move_horiz game (a, b) =
    SqSet.mem (a, b) game.board && SqSet.mem (a, b+1) game.board

let can_move_vert  game (a, b) =
    SqSet.mem (a, b) game.board && SqSet.mem (a+1, b) game.board

let c_after_horiz_move game (a, b) =
  {game with
     board = SqSet.filter (fun sq -> sq <> (a, b) && sq <> (a, b+1)) game.board;
     is_new = false;
     last_move = Some ((a, b), (a, b+1))}

let c_after_vert_move game (a, b) =
  {game with
     board = SqSet.filter (fun sq -> sq <> (a, b) && sq <> (a+1, b)) game.board;
     is_new = false;
     last_move = Some ((a, b), (a+1, b))}


let rec list_of_set set =
    let x = ref []
    in
    SqSet.iter (fun sq -> x := sq :: !x) set;
    !x


let get_last_move opt =
    match opt.last_move with
        | Some x -> x
        | None -> ((0, 0), (0, 0))

let c_options_for_game game =
    let full_square_list = list_of_set game.board
    in
    let square_list = if game.is_new
        then List.filter (fun (a, b) -> a <= (game.height+1) / 2 && b <= (game.width+1) / 2) full_square_list
        else full_square_list
    in
    let horiz_moves = List.filter (can_move_horiz game) square_list
    and vert_moves = List.filter (can_move_vert game) square_list
    in
    shuffle @@ (List.map (c_after_horiz_move game) horiz_moves) @ (List.map (c_after_vert_move game) vert_moves)


let rec pick_off_region board square set_ref =
    if SqSet.mem square !set_ref
        then set_ref
        else 

   (set_ref := SqSet.add square !set_ref;
    let (a, b) = square in
    over [(a, b+1); (a, b-1); (a+1, b); (a-1, b)] (fun sq ->
        if SqSet.mem sq board
            then ignore (pick_off_region board sq set_ref)
    );
    set_ref)


let does_have_at_most_two_neighbors (a, b) set =
    let count = ref 0
    in
    over [(a, b+1); (a, b-1); (a+1, b); (a-1, b)] (fun sq ->
        if SqSet.mem sq set then incr count);
    !count <= 2

let is_row_missing game r =
    let any = ref false in
    for c = 1 to game.width do
        any := !any || SqSet.mem (r, c) game.board
    done;
    !any

let is_column_missing game c =
    let any = ref false in
    for r = 1 to game.height do
        any := !any || SqSet.mem (r, c) game.board
    done;
    !any

let should_try_to_split game =
    if SqSet.cardinal game.board < 5 || game.last_move == None
        then false
        else

    match game.last_move with
        | None -> false
        | Some ((a, b), (c, d)) ->
            does_have_at_most_two_neighbors (a, b) game.board
         || does_have_at_most_two_neighbors (c, d) game.board
(*            is_row_missing game a || (a <> c && is_row_missing game c)
         || is_column_missing game b || (b <> d && is_column_missing game d) *)


let rec set_map fn set =
    if SqSet.is_empty set
        then SqSet.empty
        else
            let elt = SqSet.choose set
            in
            SqSet.add (fn elt) (set_map fn (SqSet.remove elt set))

let renormalize_game game =
    if true then game else

    let min_row = SqSet.fold (fun (r2, _) r -> min r r2) game.board 1000
    and min_col = SqSet.fold (fun (_, c2) c -> min c c2) game.board 1000
    and max_row = SqSet.fold (fun (r2, _) r -> max r r2) game.board 0
    and max_col = SqSet.fold (fun (_, c2) c -> max c c2) game.board 0
    in
    {game with
        board  = set_map (fun (r, c) -> (r - min_row + 1, c - min_col + 1)) game.board;
        height = max_row - min_row + 1;
        width  = max_col - min_col + 1}


let c_split game =
    if not (should_try_to_split game) then None else

    let square = SqSet.choose game.board
    in
    let region = !(pick_off_region game.board square (ref SqSet.empty))
    in
    if SqSet.cardinal region == SqSet.cardinal game.board
        then None
        else let remains = SqSet.diff game.board region
             in if SqSet.cardinal region <= SqSet.cardinal remains
                then Some (renormalize_game {game with board = region},
                           renormalize_game {game with board = remains})
                else Some (renormalize_game {game with board = remains},
                           renormalize_game {game with board = region})


let c_would_split game =
    if not (should_try_to_split game) then false else

    let square = SqSet.choose game.board
    in
    let region = !(pick_off_region game.board square (ref SqSet.empty))
    in
    if SqSet.cardinal region == SqSet.cardinal game.board
        then false
        else true


let min_breadth game =
    let rows = Array.make (game.height+1) 0
    and cols = Array.make (game.width+1) 0
    in
    SqSet.iter (fun (r, c) -> rows.(r) <- rows.(r) + 1; cols.(c) <- cols.(c) + 1) game.board;
    let minimum = ref 1000 in
    Array.iter (fun elt -> if elt > 0 then minimum := min !minimum elt) rows;
    Array.iter (fun elt -> if elt > 0 then minimum := min !minimum elt) cols;
    !minimum

let calculate_center game =
    let (x_sum, y_sum) =
        SqSet.fold (fun (a, b) (c, d) -> (a + c, b + d))
                   game.board
                   (0, 0)
    and count = SqSet.cardinal game.board
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


let is_row_or_column_missing game =
    let (r1, c1), (r2, c2) = get_last_move game
    in
    is_row_missing game r1 || (r1 <> r2 && is_row_missing game r2) ||
    is_column_missing game c1 || (c1 <> c2 && is_column_missing game c2)


let c_sorted_options game =
    let opts = c_options_for_game game
    in
    let dec = List.map (fun opt -> (distance_of_last_move_from_center opt, opt)) opts
    in
    let s = List.sort (fun (a, b) (c, d) -> a - c) dec
    in
    let y = List.map (fun (a, b) -> b) s
    in
    (* pull_to_front is_row_or_column_missing y *)
    (* shuffle opts *)
    pull_to_front (fun opt -> c_would_split opt) y

let c_hasher game =
    let arr = Array.make (game.height + 1) 0 in
    SqSet.iter
        (fun (r, c) -> arr.(r) <- arr.(r) lor (1 lsl c))
        game.board;
    arr

let _ =
    let a = int_of_string Sys.argv.(1) and b = int_of_string Sys.argv.(2)
    in
    let fn = if a > 0 && b > 0 && (a land 1) + (b land 1) = 1
                then nonzero_nimber_of_game
                else nimber_of_game
    in
    let (nimber, time) = with_time
            (fun () -> fn (c_new_game a b) c_sorted_options c_split c_hasher)
    in
    Printf.printf "%d x %d: %d  (%.2f sec, %d positions, %d HT hits)\n%!" a b nimber time !call_counter !hit_counter

