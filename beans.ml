
open Nimcalc

let push_onto list_ref elt =
    list_ref := elt :: !list_ref

let b_options (x, y) =
    let options = ref [] in
    for i = 0 to x-1 do
        push_onto options (i, y)
    done;
    for j = 0 to y-1 do
        push_onto options (x, j)
    done;
    for k = 1 to (min x y) do
        push_onto options (x-k, y-k)
    done;
    !options

let _ =
    let x = int_of_string Sys.argv.(1)
    and y = int_of_string Sys.argv.(2)
    in
    let (nimber, time) = with_time
            (fun () -> nimber_of_game (x, y) b_options (fun _ -> None) (fun x -> x))
    in
    Printf.printf "(%d, %d): %d  (took %.2f sec and %d positions)\n%!" x y nimber time !call_counter

