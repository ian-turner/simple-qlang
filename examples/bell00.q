bell00 x = let (a, b) = (init (), init ()) in (cnot (hgate a) b)
output = let (a, b) = bell00 () in (meas a, meas b)
