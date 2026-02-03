x : Qubit
f : Qubit -> (Qubit -> Qubit) -> Qubit -> (Qubit, Qubit)
f = (\ x -> g x)
