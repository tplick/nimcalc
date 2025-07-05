
open Nimcalc

type queen_game = {board: (int * int) list; n: int; is_new: bool; size: int}

let q_new_game n =
    let set = ref []
    in
    for i = 1 to n do
        for j = 1 to n do
            set := (i, j) :: !set
        done
    done;
    {board = !set; n = n; is_new = true; size = n*n}

let are_squares_on_line (a, b) (c, d) =
    let rd = a-c and cd = b-d
    in
    rd == 0 || cd == 0 || abs rd == abs cd

let q_after_move game sq1 =
    let board = ref game.board and new_board = ref [] and count = ref 0 in
    while !board <> [] do
        let sq2 = List.hd !board in
        board := List.tl !board;
        if not (are_squares_on_line sq1 sq2) then
           (new_board := sq2 :: !new_board;
            incr count)
    done;
    {board = List.rev !new_board;
     n = game.n;
     is_new = false;
     size = !count}

let q_renormalize (a, b) game =
    let xmin = List.fold_left min 1000 (List.map (fun (x, y) -> x) game.board)
    and ymin = List.fold_left min 1000 (List.map (fun (x, y) -> y) game.board)
    in
    if xmin > 0 && ymin > 0
        then {game with board = List.map (fun (x, y) -> (x - xmin, y - ymin)) game.board}
        else game

let is_one_or_two = function
    | x::[] -> true
    | x::y::[] -> true
    | _ -> false

let rec are_no_squares_aligned = function
    | [] -> true
    | x :: rest ->
        if List.exists (are_squares_on_line x) rest
            then false
            else are_no_squares_aligned rest

let rec are_all_squares_aligned = function
    | [] -> true
    | x :: rest ->
        if List.exists (fun y -> not @@ are_squares_on_line x y) rest
            then false
            else are_all_squares_aligned rest

let q_empty = {board = []; n = 0; is_new = false; size = 0}

let rec length_at_most list len =
    match (list, len) with
        | (x::xs, 0) -> false
        | ([], _) -> true
        | (x::xs, _) -> length_at_most xs (len - 1)

let q_options_for_game game =
    if are_no_squares_aligned game.board
        then (if List.length game.board land 1 = 0 then [] else [q_empty])
        else
(*    if are_all_squares_aligned game.board
        then [q_empty]
        else *)
    let moves =
(*        if is_one_or_two game.board
            then [List.hd game.board]
            else  *)
        if game.is_new
            then List.filter (fun (a, b) -> a <= b && b <= (game.n+1)/2) game.board
            else game.board
    in
    List.sort (fun a b -> a.size - b.size)
                @@ List.map (fun move -> q_after_move game move) moves

let is_first_square_isolated = function
    | [] -> false
    | x :: xs -> not @@ List.exists (are_squares_on_line x) xs

let q_splitter game =
    let len = List.length game.board
    in
    if len > 1 && len < 7 && is_first_square_isolated game.board
        then Some ({game with board = [List.hd game.board]}, {game with board = List.tl game.board})
        else None

let _ =
    let n = int_of_string Sys.argv.(1)
    in
    let (nimber, time) = with_time
            (fun () -> (if n mod 2 = 1 then nonzero_nimber_of_game else nimber_of_game)
                            (q_new_game n) q_options_for_game (fun _ -> None) (fun x -> x))
    in
    Printf.printf "%d: %d  (took %.2f sec and %d positions)\n%!" n nimber time !call_counter

