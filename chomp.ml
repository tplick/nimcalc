
open Nimcalc

let push_onto list_ref elt =
    list_ref := elt :: !list_ref

let ch_options (x, y, z) =
    let options = ref [] in
    for i = 0 to z-1 do
        push_onto options (x, y, i)
    done;
    for j = 0 to y-1 do
        push_onto options (x, j, min j z)
    done;
    for k = 1 to x-1 do
        push_onto options (k, min k y, min k z)
    done;
    OptionList !options

let _ =
    let x = int_of_string Sys.argv.(1)
    and y = int_of_string Sys.argv.(2)
    and z = int_of_string Sys.argv.(3)
    in
    let (nimber, time) = with_time
            (fun () -> nimber_of_game (x, y, z) ch_options (fun _ -> None) (fun x -> x) (fun _ -> ()))
    in
    Printf.printf "(%d, %d, %d): %d  (took %.2f sec and %d positions)\n%!"
        x y z
        nimber
        time
        !call_counter

