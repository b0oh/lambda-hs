(\increment ->
(\num ->
  increment num)
(\n i -> n (n (n i))))
(\nat n i -> n (nat n i))
