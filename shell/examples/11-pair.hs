let
  pair_make first second = \pair -> pair first second
  pair_first pair = pair (\first second -> first)
  pair_second pair = pair (\first second -> second)

  nat_zero = \next init -> init
  nat_increment nat = \next init -> next (nat next init)
  nat_multiply nat1 nat2 = \next init -> nat1 (nat2 next) init

  n1 = nat_increment nat_zero
  n2 = nat_increment n1
  n4 = nat_multiply n2 n2

  nums = pair_make n4 n2
  swap pair = pair_make (pair_second pair) (pair_first pair)
in
swap nums
