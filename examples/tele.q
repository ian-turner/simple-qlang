ctrlX phi x =
  if x then
    xgate phi
  else
    phi

ctrlZ phi x =
  if x then
    zgate phi
  else
    phi

bell00 x =
  let
    a = init ()
    b = init ()
  in (cnot (hgate a) b)

tele phi =
  let
    (a, b) = bell00 ()
    (phi, a) = cnot phi a
    phi = hgate phi
    b = ctrlX a b
    b = ctrlZ phi b
  in b

output =
  let
    a = init ()
    a = hgate a
    a = tele a
  in (meas a)
