let dump (a : Data.t) (path : String.t) : unit =
  let oc = open_out_bin path in
  Marshal.to_channel oc a [];
  close_out oc
