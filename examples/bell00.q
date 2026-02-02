defn bell00 (\ _ -> let (x, y) = (init (), init ()) in (cnot (hgate x, y)))
let (x, y) = bell00 in (meas x, meas y)
