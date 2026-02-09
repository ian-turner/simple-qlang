bell00 x =
  let a = init () in
  let b = init () in
  (cnot (hgate a) b)

output =
  let (a, b) = (bell00 ())
  in (meas a, meas b)
