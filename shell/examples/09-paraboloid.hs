let
  nat_zero = \next init -> init
  nat_increment nat = \next init -> next (nat next init)
  nat_plus nat1 nat2 = \next init -> nat1 next (nat2 next init)
  nat_multiply nat1 nat2 = \next init -> nat1 (nat2 next) init

  paraboloid x y = nat_plus (nat_multiply x x) (nat_multiply y y)

  n1 = nat_increment nat_zero
  n2 = nat_increment n1
  n4 = nat_multiply n2 n2
  n16 = nat_multiply n4 n4
in
paraboloid n16 n4
