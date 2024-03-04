open StdlibExt
open Basic
open Basic_domain
open Basic_collection
open Value_domain
open L0.CFA

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

let ( <|> ) = Angstrom.( <|> )
let ( >>= ) = Angstrom.( >>= )
let ( >>| ) = Angstrom.( >>| )
let ( *> ) = Angstrom.( *> )
let ( <* ) = Angstrom.( <* )

let ws =
  Angstrom.skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let lex c = ws *> c <* ws

let parse_hex =
  Angstrom.string "0x"
  *> Angstrom.take_while1 (function
       | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
       | _ -> false)
  >>| fun s -> Int64.of_string ("0x" ^ s)

let parse_num =
  Angstrom.take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| int_of_string

let parse_loc : Loc.t Angstrom.t =
  parse_hex >>= fun n ->
  Angstrom.char ':' *> parse_num >>| fun l -> (n, l)

let parse_step =
  lex
    (Angstrom.char 's' *> Angstrom.return Step
    <|> Angstrom.string "step" *> Angstrom.return Step)

let parse_continue =
  lex
    (Angstrom.char 'c' *> Angstrom.return Continue
    <|> Angstrom.string "continue" *> Angstrom.return Continue)

let parse_quit =
  lex
    (Angstrom.char 'q' *> Angstrom.return Quit
    <|> Angstrom.string "quit" *> Angstrom.return Quit)

let parse_show = lex (Angstrom.string "show") *> parse_loc >>| fun l -> Show l

let parse_rlist =
  lex
    (Angstrom.string "rlist" *> Angstrom.return Reachable
    <|> Angstrom.string "reachable" *> Angstrom.return Reachable)

let parse_blist =
  lex
    (Angstrom.string "blist" *> Angstrom.return BList
    <|> Angstrom.string "breakpoints" *> Angstrom.return BList)

let parse_badd = lex (Angstrom.string "badd") *> parse_loc >>| fun l -> BAdd l

let parse_bremove =
  lex (Angstrom.string "bremove") *> parse_num >>| fun n -> BRemove n

let parse_cmd : repl_cmd Angstrom.t =
  parse_step <|> parse_continue <|> parse_quit <|> parse_show <|> parse_rlist
  <|> parse_blist <|> parse_badd <|> parse_bremove

let rec repl (p : L0.Prog.t) (c : Immutable.t) (ls : Loc.t List.t)
    (bs : Loc.t List.t) (continue : Bool.t) : Unit.t =
  let cmd =
    if continue then Step
    else (
      Format.printf ">> %!";
      Option.bind (In_channel.input_line In_channel.stdin) (fun s ->
          Angstrom.parse_string ~consume:Angstrom.Consume.All parse_cmd s
          |> Result.to_option)
      |> Option.value ~default:Ignore)
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
      Format.printf "Abs State:\n%a\n" L0.FSAbsD.pp_loc (c.abs_state, l);
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
        (L0.FSAbsD.AbsLocMapD.to_list c.abs_state.pre_state |> List.map fst);
      repl p c ls bs continue
  | Ignore -> repl p c ls bs continue

let repl_analysis (p : L0.Prog.t) (e : Addr.t) : Unit.t =
  repl p (Immutable.init p (e, 0)) ((e, 0) :: []) [] false
