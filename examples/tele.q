bell00 x =
  let a = init () in
  let b = init () in
  (cnot (hgate a) b)

tele phi =
  let (a, b) = bell00 () in
  let (phi, a) = (cnot phi a) in
  let phi = (hgate phi) in
  let b = (c_x a b) in
  let b = (c_z phi b) in
  b

output =
  let a = hgate (init ())
  in (meas (tele a))
