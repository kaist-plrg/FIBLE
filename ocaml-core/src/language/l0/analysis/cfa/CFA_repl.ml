open StdlibExt
open Basic
open Basic_domain
open Basic_collection
open Value_domain
open CFA

type repl_cmd =
  | Quit
  | Step
  | Show of Loc.t
  | Reachable
  | Continue
  | Ignore
  | BList
  | BAdd of Loc.t
  | BRemove of Int.t

let parse_cmd (raw_line : string option) : repl_cmd =
  let parser =
    Parser.from_list
      [
        Parser.from_format "s" (fun x -> x Step);
        Parser.from_format "c" (fun x -> x Continue);
        Parser.from_format "q" (fun x -> x Quit);
        Parser.from_format "show %r" (fun x -> x Loc.scan (fun l -> Show l));
        Parser.from_format "rlist" (fun x -> x Reachable);
        Parser.from_format "blist" (fun x -> x BList);
        Parser.from_format "badd %r" (fun x -> x Loc.scan (fun l -> BAdd l));
        Parser.from_format "bremove %d" (fun x -> x (fun n -> BRemove n));
      ]
      Ignore
  in
  match raw_line with None -> Quit | Some s -> parser s

let rec repl (p : Prog.t) (c : Immutable.t) (ls : Loc.t List.t)
    (bs : Loc.t List.t) (continue : Bool.t) : Unit.t =
  let cmd =
    if continue then Step
    else (
      Format.printf ">> %!";
      In_channel.input_line In_channel.stdin |> parse_cmd)
  in
  match cmd with
  | Quit -> ()
  | Continue -> repl p c ls bs true
  | Step -> (
      match ls with
      | [] -> Format.printf "Empty worklist\n%!"
      | l :: ls ->
          if continue && List.mem l bs then (
            Format.printf "Breakpoint %a\n%!" Loc.pp l;
            repl p c ls bs false)
          else
            let nc, newLs = Immutable.post_worklist p c l in
            repl p nc
              (ls @ List.filter (fun l -> not (List.mem l ls)) newLs)
              bs continue)
  | Show l ->
      Format.printf "Abs State:\n%a\n" FSAbsD.pp_loc (c.abs_state, l);
      repl p c ls bs continue
  | BList ->
      Format.printf "Breakpoints:\n%a\n"
        (Format.pp_print_list Loc.pp ~pp_sep:Format.pp_print_space)
        bs;
      repl p c ls bs continue
  | BAdd l ->
      Format.printf "Breakpoint added: %a\n%!" Loc.pp l;
      repl p c ls (l :: bs) continue
  | BRemove n ->
      Format.printf "Breakpoint removed: %a\n%!"
        (Format.pp_print_option Loc.pp)
        (List.nth_opt bs n);
      let removed_elem = List.nth_opt bs n in
      let new_bs = List.filter (fun l -> Option.some l <> removed_elem) bs in
      repl p c ls new_bs continue
  | Reachable ->
      Format.printf "Reachable:\n%a\n"
        (Format.pp_print_list Loc.pp ~pp_sep:Format.pp_print_space)
        (FSAbsD.AbsLocMapD.to_list c.abs_state.pre_state |> List.map fst);
      repl p c ls bs continue
  | Ignore -> repl p c ls bs continue

let repl_analysis (p : Prog.t) (e : Addr.t) : Unit.t =
  repl p (Immutable.init p (e, 0)) ((e, 0) :: []) [] false
