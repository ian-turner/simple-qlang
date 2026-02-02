bell00 u = let (x, y) = (init (), init ()) in (cnot (hgate x, y))
main = (\ u -> (let (x, y) = (bell00 ()) in (meas x, meas y)))
