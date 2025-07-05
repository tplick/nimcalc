

let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Unix.fork() with
  | 0 ->
    Unix.close input;
    let output = Unix.out_channel_of_descr output in
    Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
    exit 0
  | _ ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        match Marshal.from_channel input with
        | `Res x -> x
        | `Exn e -> raise e


let par_array_map (f : 'a -> 'b) a : 'b array =
  Array.map (invoke f) a |>
      Array.map (fun f -> f ())


let rec split_take_n list n acc =
    if n == 0 || list == [] then (acc, list)
    else
        match list with
            | (x :: xs) -> split_take_n xs (n-1) (x :: acc)
            | [] -> raise (Failure "impossible")


let rec par_list_exists fn list length procs verbose =
    if procs = 1
        then List.exists fn list
        else

    if list == []
        then false
        else

    let these, rest = split_take_n list procs []
    in

   (if verbose then Printf.printf "  %d left...\n%!" length);
    let bools = par_array_map fn (Array.of_list these)
    in

    List.exists (fun x -> x) (Array.to_list bools)
            || par_list_exists fn rest (length - procs) procs verbose


let exit_with_failure () =
    raise (Failure "subprocess exited abnormally")


let fork_child fn elt =
    match Unix.fork () with
        | 0 -> let result = if fn elt then 1 else 0
               in Unix._exit result
        | pid -> pid


let push_onto list_ref elt =
    list_ref := elt :: !list_ref


let par_list_exists_new fn list length max_procs verbose =
    if max_procs = 1
        then List.exists fn list
        else

    if list == []
        then false
        else

    let any = ref false and active_procs = ref [] and elements_left = ref (List.length list) in
    let handle_child = (fun () -> match Unix.wait () with
                        | (pid2, Unix.WEXITED exit_code) ->
                                                (any := (!any || (exit_code == 1));
                                                 active_procs := List.filter ((<>) pid2) !active_procs;
                                                 decr elements_left;
                                                 if verbose then Printf.printf "  %d left...   \r%!" !elements_left)
                        | _ -> exit_with_failure ())
    in
    if verbose then Printf.printf "  %d left...   \r%!" !elements_left;
    List.iter (fun elt ->
        if !any
            then ()
            else (
                let pid = fork_child fn elt in
                push_onto active_procs pid;
                if List.length !active_procs >= max_procs
                    then handle_child ()
            )) list;
    while (not !any) && !active_procs <> [] do
        handle_child ()
    done;
    List.iter (fun pid -> (try Unix.kill pid 15 with _ -> ()); ignore (Unix.wait ())) !active_procs;
    if verbose then Printf.printf "\n%!";
    !any

