let
  increment nat = \n i -> n (nat n i)
  num = \n i -> n (n (n i))
in
increment (increment num)
