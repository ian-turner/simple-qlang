ctrlX x phi =
  if (meas x) then
    xgate phi
  else
    phi

ctrlZ x phi =
  if (meas x) then
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
    b = ctrlX b a
    b = ctrlZ b phi
  in b

output =
  let
    a = init ()
    a = hgate a
    a = tele a
  in (meas a)
