let
  pair_make first second = \pair -> pair first second
  pair_first pair = pair (\first second -> first)
  pair_second pair = pair (\first second -> second)

  nat_zero = \next init -> init
  nat_increment nat = \next init -> next (nat next init)
  nat_plus nat1 nat2 = \next init -> nat1 next (nat2 next init)
  nat_multiply nat1 nat2 = \next init -> nat1 (nat2 next) init
  nat_power nat1 nat2 = nat2 nat1

  nat_decrement nat =
    \next init ->
      nat
        (\g h -> h (g next))
        (\x -> init)
        (\x -> x)

  nat_decrement' nat =
    let
      paired_next nat =
        pair_make (pair_second nat) (nat_increment (pair_second nat))
    in
    pair_first (nat paired_next (pair_make nat_zero nat_zero))

  n1 = nat_increment nat_zero
  n2 = nat_plus n1 n1
  n4 = nat_multiply n2 n2
  n256 = nat_power n4 n4
in
nat_decrement n256
