let
  true x y = x
  false x y = y

  fix f = let g x = f (x x) in g g

  nat_zero = \next init -> init
  nat_increment nat = \next init -> next (nat next init)
  nat_plus nat1 nat2 = \next init -> nat1 next (nat2 next init)
  nat_multiply nat1 nat2 = \next init -> nat1 (nat2 next) init
  nat_decrement nat =
    \next init ->
      nat
        (\g h -> h (g next))
        (\x -> init)
        (\x -> x)
  nat_is_zero nat =
    nat (\x -> false) true

  n1 = nat_increment nat_zero
  n2 = nat_plus n1 n1
  n4 = nat_multiply n2 n2

  factorial =
    fix (\fac num ->
      (nat_is_zero num)
        n1
        (nat_multiply num (fac (nat_decrement num))))
in
factorial n4
