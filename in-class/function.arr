

# fun plus(n :: Number, m :: Number) -> Number:
#   n + m
# end

# fun plus(n :: Number) -> (Number -> Number):
#   lam(m): n + m end
# end


# curried
plus :: (Number -> (Number -> Number)) =
  lam(n): lam(m): n + m end end

l :: List<Number> = [list: 1, 2, 3]

check:
  # map(lam(m): plus(1, m) end, l) is [list: 2, 3, 4]

  map(plus(1), l) is [list: 2, 3, 4]
end

# plus(1) :: Number -> Number

print(plus(1)(2))


fun curry<A, B, C>(f :: (A, B -> C)) -> (A -> (B -> C)):
  lam(a):
    lam(b):
      f(a, b)
    end
  end
end

fun uncurry<A, B, C>(f :: (A -> (B -> C))) -> (A, B -> C):
  ...
end