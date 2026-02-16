bell00 x =
    let a = init ()
        b = init ()
    in (cnot (hgate a) b)

tele phi =
    let (a, b) = bell00 ()
        (phi, a) = cnot phi a
        phi = hgate phi
        b = c_x a b
        b = c_z phi b
    in b

output =
    let a = init ()
        a = hgate a
        a = tele a
    in (meas a)
