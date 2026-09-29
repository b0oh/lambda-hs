let
  true = \x y -> x
  false = \x y -> y

  and pred1 pred2 = pred1 pred2 pred1
  or pred1 pred2 =  pred1 pred1 pred2

  if_ pred then_clause else_clause =
    pred then_clause else_clause

  not pred = if_ pred false true

  id x = x -- same as unit (tuple 0)
  const x y = x
  nothing x y = y -- same as zero and false (const id)

  -- ($) x y = x y -- same as one
  -- (&) x y = y x -- same as single (tuple 1)

  zero = \next init -> init
  one = \next init -> next init
  is_zero num = num (const false) true

  pair_make first second = \pair -> pair first second
  pair_first pair = pair (\first second -> first)
  pair_second pair = pair (\first second -> second)

  triple_make first second third = \triple -> triple first second third
  triple_first triple = triple (\first second third -> first)
  triple_second triple = triple (\first second third -> second)
  triple_third triple = triple (\first second third -> third)

  is_empty term = term (\x y z -> false) true

  empty = nothing
  list = pair_make one nil
in
pair_make (is_empty list) (is_empty empty)
