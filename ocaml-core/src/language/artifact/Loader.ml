let load (path : String.t) : Data.t =
  let ic = open_in_bin path in
  let p = Marshal.from_channel ic in
  close_in ic;
  p
