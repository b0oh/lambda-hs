let
  true x y = x
  false x y = y
  and pred1 pred2 = pred1 pred2 pred1

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
  nat_monus nat1 nat2 = nat2 nat_decrement nat1
  nat_minus = nat_monus

  nat_is_zero nat = nat (\x -> false) true

  nat_is_leq_naive nat1 nat2 = nat_is_zero (nat_monus nat1 nat2)
  nat_is_eq_naive nat1 nat2 = and (nat_is_leq_naive nat1 nat2) (nat_is_leq_naive nat2 nat1)

  climb_down nat result = nat (\pred cont -> cont pred) (\x -> result)

  nat_is_lesser_or_equal nat1 nat2 = climb_down nat1 true (climb_down nat2 false)
  nat_is_equal nat1 nat2 = and (nat_is_lesser_or_equal nat1 nat2) (nat_is_lesser_or_equal nat2 nat1)
  nat_is_lesser nat1 nat2 = nat_is_lesser_or_equal (nat_increment nat1) nat2
  nat_is_greater_or_equal nat1 nat2 = not (nat_is_lesser nat1 nat2)
  nat_is_greater nat1 nat2 = not (nat_is_lesser_or_equal nat1 nat2)

  nat_min nat1 nat2 = (nat_is_lesser_or_equal nat1 nat2) nat1 nat2
  nat_max nat1 nat2 = (nat_is_lesser_or_equal nat1 nat2) nat2 nat1

  n1 = nat_increment nat_zero
  n2 = nat_plus n1 n1
  n4 = nat_multiply n2 n2
  n6 = nat_plus n4 n2
  n256 = nat_power n4 n4
in
nat_minus n256 n6
