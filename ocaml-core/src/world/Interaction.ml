let recvbuf = Bytes.create 2048
let sendbuf = Bytes.create 2048
let send_offset = ref 0

let get_char (fd : Unix.file_descr) : char =
  let recv_num = Unix.recv fd recvbuf 0 1 [] in
  assert (recv_num = 1);
  Bytes.get recvbuf 0

(* big endian *)
let get_int (fd : Unix.file_descr) : int32 =
  let recv_num = Unix.recv fd recvbuf 0 4 [] in
  assert (recv_num = 4);
  let s = Bytes.sub_string recvbuf 0 4 in
  let i = ref 0l in
  for j = 0 to 3 do
    i :=
      Int32.logor !i
        (Int32.shift_left (Int32.of_int (Char.code s.[j])) (8 * (3 - j)))
  done;
  !i

let read_int (fd : Unix.file_descr) : int32 =
  let read_num = Unix.read fd recvbuf 0 4 in
  assert (read_num = 4);
  let s = Bytes.sub_string recvbuf 0 4 in
  let i = ref 0l in
  for j = 0 to 3 do
    i :=
      Int32.logor !i
        (Int32.shift_left (Int32.of_int (Char.code s.[j])) (8 * (3 - j)))
  done;
  !i

let get_long (fd : Unix.file_descr) : int64 =
  let _ = Unix.recv fd recvbuf 0 8 [] in
  let s = Bytes.sub_string recvbuf 0 8 in
  let i = ref 0L in
  for j = 0 to 7 do
    i :=
      Int64.logor !i
        (Int64.shift_left (Int64.of_int (Char.code s.[j])) (8 * (7 - j)))
  done;
  !i

let read_long (fd : Unix.file_descr) : int64 =
  let _ = Unix.read fd recvbuf 0 8 in
  let s = Bytes.sub_string recvbuf 0 8 in
  let i = ref 0L in
  for j = 0 to 7 do
    i :=
      Int64.logor !i
        (Int64.shift_left (Int64.of_int (Char.code s.[j])) (8 * (7 - j)))
  done;
  !i

let get_string (fd : Unix.file_descr) : string =
  let len = Int32.to_int (get_int fd) in
  let s = Bytes.create len in
  let recv_num = Unix.recv fd s 0 len [] in
  assert (recv_num = len);
  Bytes.unsafe_to_string s

let read_string (fd : Unix.file_descr) : string =
  let len = Int32.to_int (read_int fd) in
  let s = Bytes.create len in
  let recv_num : int ref = ref 0 in
  while !recv_num < len do
    recv_num := !recv_num + Unix.read fd s !recv_num (len - !recv_num)
  done;
  Bytes.unsafe_to_string s

let put_char (c : char) =
  Bytes.set sendbuf !send_offset c;
  send_offset := !send_offset + 1

let put_int (i : int32) =
  for j = 0 to 3 do
    Bytes.set sendbuf (!send_offset + j)
      (Char.chr
         (Int32.to_int
            (Int32.logand (Int32.shift_right_logical i (8 * (3 - j))) 0xffl)))
  done;
  send_offset := !send_offset + 4

let put_long (i : int64) =
  for j = 0 to 7 do
    Bytes.set sendbuf (!send_offset + j)
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical i (8 * (7 - j))) 0xffL)))
  done;
  send_offset := !send_offset + 8

let put_string (s : string) =
  let len = String.length s in
  put_int (Int32.of_int len);
  for i = 0 to len - 1 do
    Bytes.set sendbuf (!send_offset + i) s.[i]
  done;
  send_offset := !send_offset + len

let flush (fd : Unix.file_descr) =
  let send_num = Unix.send fd sendbuf 0 !send_offset [] in
  assert (send_num = !send_offset);
  send_offset := 0
