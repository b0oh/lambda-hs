(\increment num ->
  increment num)
(\nat n i -> n (nat n i))
(\n i -> n (n (n i)))
