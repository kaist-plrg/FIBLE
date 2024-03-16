open Common
open L3
open L3.Sem
open State

type show_type =
  | ShowReg
  | ShowStack
  | ShowCurInst
  | ShowCont
  | ShowBlock of Loc.t
  | ShowMem of (Int.t * Value.t)
  | ShowString of Value.t

type repl_cmd =
  | Quit
  | Step
  | Show of show_type
  | Continue
  | Ignore
  | BList
  | BAdd of Loc.t
  | BAdds of String.t
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

let parse_identifier : String.t Angstrom.t =
  Angstrom.take_while1 (function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '=' -> true
    | _ -> false)

let parse_loc : Loc.t Angstrom.t =
  parse_hex >>= fun n ->
  Angstrom.char ':' *> parse_num >>| fun l -> (n, l)

let parse_numval : NumericValue.t Angstrom.t =
  parse_hex >>| fun n -> Common.NumericValue.of_int64 n 8l

let parse_stack : SPVal.t Angstrom.t =
  Angstrom.char '[' *> parse_loc <* Angstrom.char '@' >>= fun lc ->
  parse_num >>= fun n ->
  Angstrom.char ']' *> Angstrom.option 0 (Angstrom.char '+' *> parse_num)
  >>| fun off ->
  { SPVal.func = lc; timestamp = Int64.of_int n; offset = Int64.of_int off }
(* [%a@%Ld]+%Ld *)

let parse_val : Value.t Angstrom.t =
  parse_numval
  >>| (fun n -> Value.of_num n)
  <|> (parse_stack >>| fun sp -> Value.sp sp)

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

let parse_show =
  lex (Angstrom.string "i r") *> Angstrom.return (Show ShowReg)
  <|> Angstrom.string "bt" *> Angstrom.return (Show ShowStack)
  <|> ( lex
          (Angstrom.string "x/" *> Angstrom.option 8 parse_num
          <* Angstrom.string "x ")
      >>= fun n ->
        parse_val >>| fun v -> Show (ShowMem (8, v)) )
  <|> ( lex (Angstrom.string "x/s ") *> parse_val >>| fun v ->
        Show (ShowString v) )
  <|> ( lex (Angstrom.string "x/i")
      *> Angstrom.option None (parse_loc >>| Option.some)
      >>| fun lo ->
        match lo with None -> Show ShowCont | Some l -> Show (ShowBlock l) )

let parse_blist =
  lex
    (Angstrom.string "i b" *> Angstrom.return BList
    <|> Angstrom.string "info break" *> Angstrom.return BList)

let parse_badd =
  lex (Angstrom.string "b " <|> Angstrom.string "break ")
  *> (Angstrom.char '*' *> parse_loc
     >>| (fun l -> BAdd l)
     <|> (parse_identifier >>| fun l -> BAdds l))

let parse_bremove =
  lex (Angstrom.string "d " <|> Angstrom.string "delete ") *> parse_num >>| fun n -> BRemove n

let parse_cmd : repl_cmd Angstrom.t =
  parse_show <|> parse_step <|> parse_continue <|> parse_quit <|> parse_blist
  <|> parse_badd <|> parse_bremove

let repl_in in_chan out_formatter p s si (bs : Loc.t List.t) (continue : Bool.t)
    =
  let rec aux s bs continue =
    let cmd =
      if continue then Step
      else (
        Format.fprintf out_formatter ">> %!";
        Option.bind (In_channel.input_line in_chan) (fun s ->
            Angstrom.parse_string ~consume:Angstrom.Consume.All parse_cmd s
            |> Result.to_option)
        |> Option.value ~default:Ignore)
    in
    match cmd with
    | Quit -> ()
    | Continue -> aux s bs true
    | Step -> (
        if continue && List.mem (Cont.get_loc s.cont) bs then (
          Format.fprintf out_formatter "Breakpoint %a\n%!" Loc.pp
            (Cont.get_loc s.cont);
          aux s bs false)
        else
          match L3.Interp.step p s with
          | Ok s -> aux s bs continue
          | Error NormalStop ->
              Format.fprintf out_formatter "Program terminated\n%!"
          | Error (FailStop e) ->
              Format.fprintf out_formatter "Error: %s\n%!" e;
              aux s bs continue)
    | Show ShowReg ->
        Format.fprintf out_formatter "Reg:\n%a\n%!" RegFile.pp s.sto.regs;
        aux s bs continue
    | Show ShowStack ->
        Format.fprintf out_formatter "Stack:\n%a\n%!" Stack.pp s.stack;
        aux s bs continue
    | Show ShowCurInst ->
        (match s.cont.remaining with
        | [] ->
            Format.fprintf out_formatter "Current instruction: %a\n%!"
              L3.Jmp.pp_full s.cont.jmp
        | i :: _ ->
            Format.fprintf out_formatter "Current instruction: %a\n%!"
              L3.Inst.pp_full i);
        aux s bs continue
    | Show ShowCont ->
        Format.fprintf out_formatter "Current continuation: %a\n%!"
          (Format.pp_print_list
             ~pp_sep:(fun fmt _ -> Format.fprintf fmt "\n")
             L3.Inst.pp_full)
          s.cont.remaining;
        Format.fprintf out_formatter "%a\n%!" L3.Jmp.pp_full s.cont.jmp;
        aux s bs continue
    | Show (ShowBlock l) ->
        let bo =
          List.fold_left
            (fun (b : Block.t Option.t) (f : Func.t) ->
              match b with Some b -> Some b | None -> Func.get_bb f l)
            None p.funcs
        in
        (match bo with
        | Some b ->
            (Format.pp_print_list
               ~pp_sep:(fun fmt _ -> Format.fprintf fmt "\n")
               L3.Inst.pp_full)
              out_formatter b.body;
            Format.fprintf out_formatter "%a\n%!" L3.Jmp.pp_full b.jmp
        | None -> Format.fprintf out_formatter "Block %a not found\n%!" Loc.pp l);
        aux s bs continue
    | Show (ShowMem (n, v)) ->
        (match Store.load_bytes s.sto v (Int32.of_int n) with
        | Ok s ->
            Format.fprintf out_formatter "Memory at %a:\n@[%a@]@,%!" Value.pp v
              (Format.pp_print_list
                 (fun fmt (b : Char.t) ->
                   Format.fprintf fmt "%02x" (Char.code b))
                 ~pp_sep:(fun fmt _ -> Format.fprintf fmt " "))
              (String.to_seq s |> List.of_seq)
        | Error e -> Format.fprintf out_formatter "Error: %s\n%!" e);
        aux s bs continue
    | Show (ShowString v) ->
        (match Store.load_string s.sto v with
        | Ok s ->
            Format.fprintf out_formatter "String at %a: %s\n%!" Value.pp v s
        | Error e -> Format.fprintf out_formatter "Error: %s\n%!" e);
        aux s bs continue
    | BList ->
        Format.fprintf out_formatter "Breakpoints:\n%a\n%!"
          (Format.pp_print_list Loc.pp ~pp_sep:Format.pp_print_space)
          bs;
        aux s bs continue
    | BAdd l ->
        Format.fprintf out_formatter "Breakpoint added: %a\n%!" Loc.pp l;
        aux s (l :: bs) continue
    | BAdds sn -> (
        let fo =
          List.find_opt
            (fun (f : Func.t) ->
              match f.nameo with Some x -> String.equal x sn | None -> false)
            p.funcs
        in
        match fo with
        | Some f ->
            Format.fprintf out_formatter "Breakpoint added: %a\n%!" Loc.pp
              f.entry;
            aux s (f.entry :: bs) continue
        | None ->
            Format.fprintf out_formatter "%s not found\n%!" sn;
            aux s bs continue)
    | BRemove n ->
        Format.fprintf out_formatter "Breakpoint removed: %a\n%!"
          (Format.pp_print_option Loc.pp)
          (List.nth_opt bs n);
        let removed_elem = List.nth_opt bs n in
        let new_bs = List.filter (fun l -> Option.some l <> removed_elem) bs in
        aux s new_bs continue
    | Ignore -> aux s bs continue
  in
  aux s bs continue

let repl ((in_chan, out_chan) : In_channel.t * Out_channel.t) (p : L3.Prog.t)
    (s : State.t) : (Unit.t, String.t) Result.t =
  repl_in in_chan (Format.formatter_of_out_channel out_chan) p s s [] false;
  Ok ()
